# Install-time utilities for basilisk

This package provides install-time utilities for the [**basilisk**](https://github.com/LTLA/basilisk) package.
I split these functions into a separate package so that they would be available while **basilisk** is being installed,
in particular for system installations where **basilisk**'s installation triggers the creation of a central Conda installation.
If these functions lived inside **basilisk** itself, I wouldn't be able to use them during **basilisk**'s own installation,
and I didn't want to maintain a shadow codebase inside **basilisk**'s `configure` script;
hence, a separate package.

**basilisk** users and developers should probably look at the [**basilisk**](https://github.com/LTLA/basilisk) homepage, unless you know what you're doing.
