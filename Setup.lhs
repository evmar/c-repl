#! /usr/bin/env runhaskell

This setup file ought to be pretty simple, but we have one extra dependency:
our "child" executable, a helper program that's written in C.  So we have to
patch in a compile and install step for the child program.

This seems conceptually simple, but the "simple" distribution system used by
Cabal et al is a huge confusing undocmented mess.  :(

> import Control.Applicative
> import Distribution.Simple
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Setup
> import Distribution.Simple.Program
> import Distribution.Simple.Utils
> import Distribution.Verbosity
> import System.FilePath ((</>))
> import System.Posix.Files

> creplChildName :: String
> creplChildName = "c-repl-child"

> creplChildPath :: LocalBuildInfo -> FilePath
> creplChildPath buildinfo = buildDir buildinfo </> creplChildName

> creplChildBuild :: Args -> BuildFlags -> PackageDescription
>                 -> LocalBuildInfo -> IO ()
> creplChildBuild args flags desc buildinfo = do
>   rawSystemProgramConf (fromFlag $ buildVerbosity flags) gccProgram (withPrograms buildinfo)
>       ["child.c", "-o", creplChildPath buildinfo, "-ldl"]

> creplChildCopy :: Args -> CopyFlags -> PackageDescription
>                -> LocalBuildInfo -> IO ()
> creplChildCopy args flags desc buildinfo = do
>   let dirs = absoluteInstallDirs desc buildinfo (fromFlag $ copyDest flags)
>   let libexec = libexecdir dirs
>   let target = libexec </> creplChildName
>   putStrLn $ "copying child to " ++ target
>   let verbosity = fromFlag $ copyVerbosity flags
>   -- You might reasonably ask, "what is this mystery True argument here?"
>   -- I have no idea; it's not documented.
>   -- I am surely doing something wrong here but I've given up.
>   createDirectoryIfMissingVerbose verbosity True libexec
>   copyFileVerbose verbosity (creplChildPath buildinfo) target
>   -- copyFile appears to lose the +x bit on the binary.
>   mode <- fileMode <$> getFileStatus target
>   let desiredMode = unionFileModes ownerExecuteMode mode
>   setFileMode target desiredMode

> buildHooks = simpleUserHooks {
>   hookedPrograms = [gccProgram],
>   postBuild = creplChildBuild,
>   postCopy = creplChildCopy
> }

> main = defaultMainWithHooks buildHooks
