# Git

This is an F# implementation of Git.

As of the first commit, it is liable to be highly inefficient; it might not close streams it opens, it might read streams to the end when it need not, and so forth.

# Why?

LibGit2Sharp is all fine and dandy except that it has to act on a real filesystem.
