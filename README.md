# Hassub
Hassub is a command-line subtitle downloader made in Haskell. It consumes [OpenSubtitles API](http://www.opensubtitles.org).

```
Usage:
  hassub -l=LANG [-s] --all
  hassub -l=LANG [-s] <file>...
  hassub --help
  hassub --version

Options:
  -l=LANG                    Subtitle language. Use ISO 639-2 codes e.g. eng (use pob to portugues br)
  --all                      Download subtitles for all movie files in current directory
  -s, --silent               Non interactive mode. Just download the subtitles with more downloads count
  -h, --help                 Show this help text
  -v, --version              Show version
```
