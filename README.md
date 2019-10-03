# spookel
prototype of research paper annotation tool

## Getting Started
```bash
cabal run
```

Then point your web browser to `localhost:3000`

All of the annotations are saved in a local directory `~/.fermatslastmargin/localuser` and your friend's annotations are saved in `~/.fermatslastmargin/friends/<github_name_of_friend>`

# Features
- [X] add paper info
- [X] read/write annotations
- [X] load page images
- [ ] render 'uploaded' PDF to page images
- [ ] push to github repo
- [ ] pull from friends' github repo
- [ ] download paper (as PDF) when given unique ID (DOI for now)
- [ ] search arxiv by title to get DOI
- [ ] search crossref.org by title to get DOI
