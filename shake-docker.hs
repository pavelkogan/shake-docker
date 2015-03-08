#!/usr/bin/env runghc

{-# LANGUAGE NoImplicitPrelude #-}

import BasePrelude
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import Safe
import Text.Printf

data DockerImage = DockerImage
    { name :: String
    , tag  :: String
    , id_  :: String
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

getProject = do
    [f] <- getDirectoryFiles "" ["*.cabal"]
    let n = takeBaseName f
    r <- getGitRevision
    return $ Project n f r

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
        need [config]
        withTempDir $ \ buildDir -> do
            writeFile' (buildDir </> "Dockerfile") ltsDockerfile
            copyFile' config $ buildDir </> "lts-cabal.config"
            let v' = "7.8-" <> v
            () <- cmd (Cwd buildDir) $ "docker build -t haskell-lts:"<>v'<>" ."
            imageId <- getImageId "haskell-lts" v'
            writeFileChanged out imageId
    "_shake/*.project.image" %> \ out -> do
        Project n _ r <- getProject
        let revision = dropDirectory1 $ dropExtension2 out
        when (r /= revision) $ error "git revision error"
        () <- cmd (printf "docker build -t %s:%s ." n r :: String)
        imageId <- getImageId n r
        writeFileChanged out imageId
    "project" ~> do
        Project n _ r <- getProject
        need [printf "_shake/%s.project.image" r]
        cmd (printf "docker tag -f %s:%s %s:%s" n r n "latest" :: String)
    "lts-versions" ~> do
        images <- getImages "haskell-lts"
        liftIO $ mapM_ (putStrLn . tag) images

getImageId :: String -> String -> Action String
getImageId n t = do
    images <- getImages n
    return $ id_ $ head $ filter ((t ==) . tag) images

getImages :: String -> Action [DockerImage]
getImages s = do
    Stdout out <- cmd $ "docker images " <> s
    return $ dockerImages out

ltsDockerfile :: String
ltsDockerfile =
    "FROM haskell:7.8\n\
    \RUN cabal update\n\
    \ADD ./lts-cabal.config /root/.cabal/lts-cabal.config\n\
    \RUN cd /root/.cabal/ && cat lts-cabal.config >> cabal.config\n\
    \RUN cabal update && cabal install cabal-install base-prelude"

dockerImages = map dockerImage . drop 1 . lines

dropExtension2 = dropExtension . dropExtension

curlConfig v = "curl http://www.stackage.org/lts" </> v </> "cabal.config?global=true"

getLtsVersion = dropWhile (not . isDigit) . head . lines
