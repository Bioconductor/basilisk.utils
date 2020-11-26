\name{basilisk.utilsNews}
\title{basilisk.utils News}
\encoding{UTF-8}

\section{Version 1.4.0}{\itemize{
\item Avoid caching the installer when performing a system installation.
Otherwise, cache in the external directory to avoid requiring/polluting \pkg{BiocFileCache}'s cache.
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

