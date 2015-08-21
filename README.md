Search and download torrents from t411.me
=========================================

**This is a really early release.**
`t411` is a command-line interface used to search and download torrents from the http://t411.me website.

The general usage is:
```
t411 ACTION USERNAME PASSWORD args...
```
Yes, you must provide your username and password on the command line.


Searching
---------

```
t411 search USERNAME PASSWORD QUERY OFFSET COUNT
```
So `USERNAME`, `PASSWORD` and `QUERY` speak for themselves. `OFFSET` and `COUNT` are needed to limit the number of returned torrents.

Example:
```bash
t411 search 'username' 'password' 'avatar' 15 10
```


Downloading
-----------

Use the id returned by the search action to download:
```
t411 download USERNAME PASSWORD TORRENTID
```

Example:
```bash
t411 download 'username' 'password' 1234
```
