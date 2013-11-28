# Git Checklist

I was juggling lots of private git branches at work and making some use
of the `BRANCH_DESCRIPTION` file (see `git branch --edit-description`)
to store my notes for each branch. I soon realise what I wanted was to
just store my TODOs in each branch and mark them off as I went.

And so was born `git-checklist`, though in actual fact I've aliased it
to `git todo`. Assuming it's in your path:

    $ git config alias.todo=checklist

Otherwise, like this:

    $ git config alias.todo=!/path/to/git-checklist

## Using It Is Easy

To view your TODOs, just enter the bare command:

    $ git todo
    key: add <description>

It will give you a summary of the commands you can enter at this point.
There are no entries yet so all you can do is add them.

    $ git todo add Add an informative README file
    key: add <description>
    1: [ ] Add an informative README file

Each entry has a number in the left column and a box. Empty boxes mean
the item is still to be done. When you've finished a task, mark it done
using the number as a reference:

    $ git todo done 1
    key: add <description> | done <n> | undo <n> | del <n>
    1: [x] Add an informative README file

You can also "undo" items if you realise that the fix you made wasn't so
hot after all, or you didn't understand the issue fully, or whatever
other reason.

Finally you can remove items entirely from the list. This is permanent
and can't be undone.

## Shortcomings

The data is stored in a separate directory inside `.git` which isn't
versioned --- this is a local list only. I am open to suggestions to
make this versioned though it is not a priority for me. My workflow is
based around a single computer.

The read/write step bashes into the awkwardness of lazy IO. The key
printed to the screen is just a quick hack to get around that.

If you have any more than 9 items in a list the pretty printing will no
longer look as pretty because only 1 character is allowed for the
numbers. Any more than that and the checkboxes no longer line up --- not
pretty!
