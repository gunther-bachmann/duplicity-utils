#lang scribble/manual

@title{duplicity utils}

@defproc[(check-backups) Unit]{
  Read backup configuration and check backups for outdated backups that could be deleted to save drive capacity
}
