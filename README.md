# Git Checklist

I was juggling lots of private git branches at work and making some use
of the branch description file (see `git branch --edit-description`)
to store my notes for each branch. This was good for me because the
descriptions aren't committed to the branch so they would never make it
upstream (even by accident). However managing the TODOs themselves was
pretty unwieldy.

And so was born `git-checklist`, though in actual fact I've aliased it
to `git todo`. Assuming it's in your path:

    $ git config alias.todo=checklist

Otherwise, like this:

    $ git config alias.todo=!/path/to/git-checklist

## Using It Is Easy

To view your TODOs, just enter the bare command from a git repository:

    $ git todo

Well, if you've not got anything to do yet, it won't have anything to
show you. Find out where to go next:

    $ git todo --help
    Usage: git-checklist [COMMAND]
      Per-branch TODO list for Git repositories

    Available options:
      -h,--help                Show this help text

    Available commands:
      show                     Show current TODOs
      add                      Add a TODO
      done                     Mark a TODO as done.
      undo                     Item needs redone!
      remove                   Remove a TODO (can't be undone)
      stats                    Summary statistics of checklist

It will give you a summary of the commands you can enter at this point.
There are no entries yet so all you can do is add them.

    $ git todo add Add an informative README file
    1: [ ] Add an informative README file

Each entry has a number in the left column and a box. Empty boxes mean
the item is still to be done. When you've finished a task, mark it done
using the number as a reference:

    $ git todo done 1
    1: [x] Add an informative README file

You can also "undo" items if you realise that the fix you made wasn't so
hot after all, or you didn't understand the issue fully, or whatever
other reason.

Finally you can remove items entirely from the list. This is permanent
and can't be undone.

## Advanced Usage

If you're knee deep in work on one branch and you'd like to change the
checklist for another branch, all the commands will recognise the
`--branch` (or `-b`) option.

    $ git todo add --branch newparser Make nicer error messages

If you want to start a description of your note with a hyphen you can
separate it from the rest of the command options with a double-hyphen on
its own.

    $ git todo add -- -b stopped working but --branch still okay

If you want to see all your branches at once use

    $ git todo show --all

Obviously you can't supply the `--all` flag to any of the editing
commands --- you can only edit one branch's checklist at a time.

There is one more overview command that will take `--branch` or `--all`
as an option:

    $ git todo stats
    2 tasks to do (4 in total)

## Work In Progress Commits

I've got an alias called `wip` which lets me quickly save all my unsaved
work in the current branch. You can use this as a basic command:

    $ git config alias.wip=commit -a -m"WIP"

But if you use git-checklist the following might be more informative
when you are trying to get the measure of your branches:

    $ git config alias.wip=!git commit -a -m\""WIP $(git todo stats)"\"

Then you'll get a commit that says something like _WIP 2 tasks to do (4
in total_ which is much nicer. For the cherry on top I also use
`prepare-commit-msg` to pipe the result of `git todo show` to the commit
message. The summary line will contain a summary and the rest of the
message will contain the current state of the branch.

## Background Details

The checklists are stored in `.git/checklist/<branchname>` in a
serialised form of the internal data structure called `ToDo`. If you
open up one of these files it's easy to edit though you might find
breakage occurs!

The checklist for the working directory is whatever branch name is
pointed to by `HEAD`. I have not tested it with a detached head, though I
guess you'd just end up with a SHA as a branch name.

## Installing It Is Fairly Easy

Assuming you've got a Haskell installation (if not, [grab the Haskell
platform] [hp]) you can grab the source and build with Cabal.

    $ git clone http://github.com/dougalstanton/git-checklist
    $ cd git-checklist
    $ cabal build
    $ cabal install

[hp]: <http://www.haskell.org/platform>
    "Haskell for Windows, Mac OS X and Linux"

## Shortcomings

The data is stored in a separate directory inside `.git` which isn't
versioned --- this is a local list only. I am open to suggestions to
make this versioned though it is not a priority for me. My workflow is
based around a single computer.

The read/write step bashes into the awkwardness of lazy IO.

The serialisation is quick-and-dirty using Haskell's `deriving
(Read,Show)` functionality which means not only is it brittle but the
error messages on failure are poor and the storage format could be
better too. In future I'd like to use a line-oriented format which is
easier to edit manually if necessary.

It doesn't cope very well with not being somewhere inside a repository.
It shouldn't wreck anything but the error message isn't very refined.

## Future Changes

The next step will be to decide on a proper storage format so I can
write a parser and pretty printer for it. I imagine that will involve
making a conversion routine to upgrade any existing todo lists. Not
least of all my own todo lists!
