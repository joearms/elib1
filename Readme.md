Elib1
===

An Erlang library and collection of applications

You will also have to do the following:

1. add a path to your `${HOME}/.erlang`
```erlang
code:add_patha(".../elib1-3/lib/ebin").
```

2. add path to your .bash_profile (on a mac)
```shell
export PATH=/Users/joe/code/elib2-1/bin:$PATH
```

3. make eindex and executable
```shell
chmod u+x elib1-3/bin/einidex
chnod u+x elib1-3/bin/elib2_find_similar_files
```

Then make to compile the library and view
the documentation.

This should start a web server on port 

If it doesn't point your browser to http://localhost:2246/supported/website/index.ehtml

Note - I have only tested this with firefox
