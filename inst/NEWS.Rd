\name{basilisk.utilsNews}
\title{basilisk.utils News}
\encoding{UTF-8}

\section{Version 1.18.0}{\itemize{
\item Switch to the latest Miniforge installer (24.3.0-0) by default.
This is preconfigured to use the conda-forge channel and avoids issues with the non-FOSS licensing of the Anaconda repositories.
Users can switch back to the old Miniconda installer by setting the \code{BASILISK_USE_MINIFORGE=0} environment variable,
but this will likely be deprecated in the next release.

\item Update the \pkg{reticulate} version in the fallback environment to 1.38.
}}

\section{Version 1.12.0}{\itemize{
\item Added Arm64 support for Linux.
}}

\section{Version 1.4.0}{\itemize{
\item Avoid caching the installer when performing a system installation in \code{installConda()}.
Otherwise, cache in the external directory to avoid requiring/polluting \pkg{BiocFileCache}'s cache.

\item Officially give up on Windows 32-bit support in \code{installConda()}.

\item Migrated \code{activateEnvironment()} back here, from \pkg{basilisk}.

\item Added \code{cleanConda()} utility to clean the Conda environment.

\item Added \code{setCondaPackageDir()} to set the Conda package cache directory.
}}

\section{Version 1.2.0}{\itemize{
\item Migrated most environment-related functions to \pkg{basilisk}.

\item Added locking to \code{installConda()} for safe parallel lazy Conda installations.

\item Switched to the latest Miniconda3 installer.
}}

\section{Version 1.0.0}{\itemize{
\item
New package \pkg{basilisk.utils}, to handle install-time configuration of \pkg{basilisk}.
}}

