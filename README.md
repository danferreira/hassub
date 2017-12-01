# Hassub
Hassub is a command-line subtitle downloader made in Haskell. It consumes [OpenSubtitles API](http://www.opensubtitles.org).

```
Usage:
  hassub -d [-l=LANG] [-s] --all
  hassub -d [-l=LANG] [-s] <file>...
  hassub --config
  hassub --help
  hassub --version

Options:
  -d, --download              Download the subtitle
  -l=LANG                     Subtitle language. Use ISO 639-2 codes e.g. eng
  -a, --all                   Download subtitles for all movie files in current directory
  -s, --silent                Non interactive mode. Just download the subtitles with more downloads count
  -c, --config                Edit the configuration file (default language, login and password)
  -h, --help                  Show this help text
  -v, --version               Show version
```
