# Analyse data collection 

To get size of folders

```bash
# 1 level hierarchy (only child folders)
$ du -sh * | sort -hr

# 2 level hierarchy (to find big files in folders)
$ du -h --max-depth=2 | sort -hr

# Top 20 biggest files/folders in current directory
$ du -h | sort -hr | head -n 20
```