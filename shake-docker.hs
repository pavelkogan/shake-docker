#!/usr/bin/env runghc

{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import Data.Version ()
import Development.Shake
import Development.Shake.FilePath
import Safe
import Text.Printf

data DockerImage = DockerImage
    { _name :: String
    , _tag  :: String
    , _id   :: String
    } deriving (Show, Read, Eq)

dockerImage :: String -> DockerImage
dockerImage = f . words where
    f (n:t:i:_) = DockerImage n t i
    f _         = error "unexpected docker image input"

data Tag = Latest | Version' {getVersion :: Version} | Other String
    deriving (Show, Read, Eq)

parseTag :: String -> Tag
parseTag s = case (s, lastMay parse) of
                 ("latest", _)     -> Latest
                 (_, Just (v, "")) -> Version' v
                 _                 -> Other s
    where
        parse = readP_to_S parseVersion s

showTag :: Tag -> String
showTag Latest       = "latest"
showTag (Version' v) = showVersion v
showTag (Other s)    = s

data Project = Project
    { projectName :: String
    , cabalFile   :: FilePath
    , gitRevision :: String
    } deriving (Eq, Show)

getProject :: Action Project
getProject = do
    [f] <- getDirectoryFiles "" ["*.cabal"]
    let n = takeBaseName f
    r <- getGitRevision
    return $ Project n f r

getGitRevision :: Action String
getGitRevision = do
    Stdout r <- cmd "git rev-parse --short HEAD"
    return $ head $ words r  -- drops newline

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_shake/"} $ do
    "clean" ~> do
        putNormal "Cleaning files in _shake/"
        removeFilesAfter "_shake" ["//*"]
    alternatives $ do
        "latest.lts-cabal.config" ~> do
            alwaysRerun
            Stdout config <- cmd $ curlConfig ""
            let current = getLtsVersion config
            writeFileChanged (current <.> "lts-cabal.config") config
        "*.lts-cabal.config" %> \out -> do
            alwaysRerun
            let v = dropExtension2 out
            Stdout config <- cmd $ curlConfig v
            writeFileChanged out config
    "_shake/*.lts-cabal.image" %> \ out -> do
        let config = dropDirectory1 $ out -<.> "config"
        let v = dropExtension2 config
        ltsDockerfile `withClean` [(config, "lts-cabal.config")] $ \ buildDir ->
            dockerBuild (Just buildDir) Nothing "haskell-lts" ("7.8-" <> v) out
    "_shake/*.cabal" %> \ out ->
        copyFileChanged (dropDirectory1 out) out
    "_shake/*.project-deps.image" %> \ out -> do
        Project n _ _ <- getProject
        d <- readFileLines "Dockerfile"
        let c = n <.> ".cabal"
            f = [("_shake" </> c, c)]
        dockerInstallDeps n d `withClean` f $ \ buildDir ->
            dockerBuild (Just buildDir) Nothing (n <> "-deps") "latest" out
    "_shake/*.project.image" %> \ out -> do
        Project n _ r <- getProject
        let revision = dropDirectory1 $ dropExtension2 out
        when (r /= revision) $ error "git revision error"
        withTempFile $ \ dockerfile -> do
            id' <- getImageId (n <> "-deps") "latest"
            writeFile' dockerfile (projectDockerfile n id')
            dockerBuild Nothing (Just dockerfile) n r out
    "project" ~> do
        Project n _ r <- getProject
        need [printf "_shake/%s.project.image" r]
        cmd (printf "docker tag -f %s:%s %s:%s" n r n "latest" :: String)
    "lts-versions" ~> do
        images <- getImages "haskell-lts"
        liftIO $ mapM_ (putStrLn . _tag) images

withClean :: String                  -- ^ Dockerfile contents
          -> [(FilePath, FilePath)]  -- ^ map files to relative path in buildDir
          -> (FilePath -> Action a)  -- ^ takes buildDir
          -> Action a
withClean d fs a = withTempDir $ \ b -> do
    writeFile' (b </> "Dockerfile") d
    forM_ fs $ \(x, y) -> copyFile' x (b </> y)
    a b

dockerBuild :: Maybe FilePath  -- ^ build directory
            -> Maybe FilePath  -- ^ optional Dockerfile
            -> String          -- ^ image name
            -> String          -- ^ image tag
            -> FilePath        -- ^ file to save image id to
            -> Action ()
dockerBuild buildDir dockerfile name tag imageIdFile = do
    let cmd' = maybe cmd (cmd . Cwd) buildDir
        df   = maybe "" ("-f " <>) dockerfile
    () <- cmd' (printf "docker build %s -t %s:%s ." df name tag :: String)
    imageId <- getImageId name tag
    writeFileChanged imageIdFile imageId

getImageId :: String         -- ^ image name
           -> String         -- ^ image tag
           -> Action String
getImageId n t' = f t' n where
    f t = return . _id . headNote e . filter ((t ==) . _tag) <=< getImages
    e   = printf "getImageId - couldn't find image %s:%s" n t'

getImages :: String -> Action [DockerImage]
getImages s = do
    Stdout out <- cmd $ "docker images " <> s
    return $ dockerImages out

dockerInstallDeps :: String -> [String] -> String
dockerInstallDeps n d = unlines $ d ++
    [ printf "ADD ./ /%s/" n
    , printf "RUN cd /%s && cabal install --dependencies-only" n
    ]

projectDockerfile :: String -> String -> String
projectDockerfile n i = unlines
    [ printf "FROM %s" i
    , printf "ADD ./ /%s/" n
    , printf "RUN cd /%s && cabal install" n
    ]

ltsDockerfile :: String
ltsDockerfile = unlines
    [ "FROM haskell:7.8"
    , "RUN cabal update"
    , "ADD ./lts-cabal.config /root/.cabal/lts-cabal.config"
    , "RUN cd /root/.cabal/ && cat lts-cabal.config >> cabal.config"
    , "RUN cabal update && cabal install cabal-install base-prelude"
    , "ENV PATH /root/.cabal/bin:$PATH"
    ]

dockerImages :: String -> [DockerImage]
dockerImages = map dockerImage . drop 1 . lines

dropExtension2 :: FilePath -> FilePath
dropExtension2 = dropExtension . dropExtension

curlConfig :: FilePath -> FilePath
curlConfig v = "curl http://www.stackage.org/lts" </> v </> "cabal.config?global=true"

getLtsVersion :: String -> String
getLtsVersion = dropWhile (not . isDigit) . headNote e . lines
    where e = "getLtsVersion"
