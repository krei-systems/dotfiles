#!/usr/bin/env -S zsh -f

#———————————————————————————————————————————————————————————————————————————————
# funkcioj

function _k {
  local dir=/mnt/"${1}"/home

  mnt "${1}" && (
    s rz -v --delete --delete-excluded \
      --exclude "chrt" \
      --exclude "lost+found" \
      /home/ $dir
  )
}

#———————————————————————————————————————————————————————————————————————————————
# ĉeffunkcioj

function k {
  local d=$(date +"%Y%m%d%H%M%S")

  if ((!$#)); then
    page $0
  else
    for target in ${@}; do
      case $target in
      (b0 | b1)
        _k "${target}"
        ;;
      (la-vulpo | v)
        rz -v --delete \
          ${HOME}/{Desktop,Developer,Documents,Downloads,Music,Pictures} \
          --exclude vix/vix \
          --exclude miera/miera \
          la-vulpo:
        ;;
      (mycloud | mc)
        rz -v --delete \
          /Volumes/ebzzry/ \
          /Volumes/la-azeno-1a/MyCloud/ebzzry/
        ;;
      (zsh | z)
        xz -z -c ${HOME}/.zhistory >${HOME}/dat/ziŝo/${d}.zhistory.xz
        ;;
      (bookmarks | bm)
        xz -z -c ${HOME}/.emacs.d/.cache/bookmarks >${HOME}/dat/emacs/${d}.bookmarks.xz
        ;;
      (docjem)
        sue docjem rsync -avz \
          --exclude ".nix*" \
          --exclude ".ssh" \
          --exclude ".Trash" \
          --exclude "Library/" \
          --exclude "MobileSync/" \
          --exclude "Music/" \
          --exclude "Containers/" \
          --exclude "Desktop/DENTAL BOOKS" \
          --delete-excluded \
          pegasus.local: ~docjem
        ;;
      esac
    done
  fi
}
