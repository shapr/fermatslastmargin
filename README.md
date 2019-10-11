# spookel
prototype of research paper annotation tool

## Getting Started
* install [poppler-utils](https://poppler.freedesktop.org/), or more specifically, make sure the pdftocairo binary from poppler-utils is in your `$PATH`
* git clone this repository
* run this from cabal `cabal run`
* point your web browser to `localhost:3000`

All of the annotations are saved in a local directory `~/.fermatslastmargin/localuser` and your friend's annotations are saved in `~/.fermatslastmargin/friends/<github_name_of_friend>`

# Features
- [X] add paper info
- [X] read/write annotations
- [X] load page images
- [X] render 'uploaded' PDF to page images
- [X] push to github repo
- [ ] pull from friends' github repo
- [ ] switch notes view to see github friends' notes
- [ ] download paper (as PDF) when given unique ID (DOI for now)
- [ ] search arxiv by title to get DOI
- [ ] search crossref.org by title to get DOI
