#!/usr/bin/env -S zsh -f

#———————————————————————————————————————————————————————————————————————————————
# ĉefaferoj

function ck {
  local cmd='/On branch/d;/nothing to commit/d;/up-to-date/d'

  for i in ${HOME}/src/ttt/ebzzry.github.io; do
    (
      cd $i
      if [[ -d .git ]]; then
        print "\e[0;32m${i:t}\e[0;0m"
        git status | sed $cmd
      fi
    )
  done

  for i in ${HOME}/etc/*; do
    (
      cd $i
      if [[ -d .git ]]; then
        print "\e[0;35m${i:t}\e[0;0m"
        git status | sed $cmd
      fi
    )
  done

}

function emv {
  if [[ $# -eq 2 ]]; then
    mute emacsclient -e "(dired-rename-file \"$1\" \"$2\" t)"
  fi
}

function ecp {
  if [[ $# -eq 2 ]]; then
    mute emacsclient -e "(dired-copy-file \"$1\" \"$2\" t)"
  fi
}

function mvid3 {
  local id_val=

  for i in *.[mM][pP]3; do
    id_val="$(id3tool $i | grep "^${1}" | awk '{print $2}')"
    if [[ -n "$id_val" ]]; then
      mkdir "$id_val"
      mv "$i" "$id_val"
    fi
  done
}

function click {
  if (( $#1 )); then
    while :; do
      xdotool click 1
      sleep $1
    done
  else
    xdotool click 1
  fi
}

function def_getter {
  local func=${1}get
  local handler=$1
  local handler_file=$2
  shift 2
  eval "function $func {
          if (( ! \$#@ )); then
              if [[ -f $handler_file ]]; then
                  $handler $handler_file
              fi
          else
             ${@} \"\${@}\"
          fi
        }"
}

function def_mapper {
  local name=$1
  eval "function $name {
          #trap 'echo trap; return 1' INT
          local line=
          if [[ \$# > 1 ]]; then
              map $name "\${@}"
          else
              if [[ -f "\$1" ]]; then
                  mapl $name "\$1"
              elif [[ -d "\$1" ]]; then
                  echo \* \$1
                  (cd "\$1"; $name)
              else
                  line="\$1"
                  if [[ ! \$line[1] == \"#\" ]]; then
                      ${1}get "\${@}"
                  fi
              fi
          fi
        }"
}

linux_test && YOUTUBE=yt-dlp
darwin_test && YOUTUBE=yt-dlp

def_getter y y.txt $YOUTUBE --cookies-from-browser firefox -c -i
def_getter ya y.txt $YOUTUBE -c -i --extract-audio --audio-quality 0 --audio-format mp3
def_getter ys y.txt $YOUTUBE --write-sub --sub-lang en --sub-format srt
#def_getter q q.txt wget -t 0 -c

#map def_mapper y ya ys q
map def_mapper y ya ys

function mplay { =mpv $([[ -f mpv.opts ]] && cat mpv.opts) "${@}" }

function media_player {
  if [[ -d "${argv[-1]}" ]]; then
    cd "${argv[-1]}" $0
  else
    if [[ "$1" == *"http"* && "$1" == *"youtu"* ]]; then
      play_offset "$1"
    elif some_files ${@}; then
      play_offset ${@}
    else
      play_rest ${@}
    fi
  fi
}

function play_offset {
  local times= start= stop=

  if [[ -f ${argv[-1]}.off ]]; then
    times=$(cat ${argv[-1]}.off)
    start=$(echo $times | cut -d ' ' -f 1)
    stop=$(echo $times | cut -d ' ' -f 2)
    if [[ ! "$start" = "$stop" ]]; then
      mplay --start=$start --stop=$stop ${@}
    else
      mplay --start=$start ${@}
    fi
  else
    mplay ${@}
  fi
}

function play_rest {
  local listfile=in.m3u
  local lastfile=in.last

  if [[ -f $lastfile ]]; then
    mplay ${@} "$(cat $lastfile)"
  elif [[ -f $listfile ]]; then
    mplay ${@} --playlist=$listfile
  else
    mplay ${@}
  fi
}

function pk {
  for name in ${@}; do
    for pid in $(pq $name | awk '{print $1}'); do
      s kill -9 $pid
    done
  done
}

function addaptkey {
  gpg --keyserver subkeys.pgp.net --recv-keys $1
  gpg --armor --export $1 | sudo apt-key add -
}

function aptselect {
  local debian_version=`cat /etc/debian_version | cut -d \/ -f 1`
  local output_file=sources.list.`date "+%Y-%m-%d-%H-%M"`

  s netselect -s -n ${debian_version} -o ${output_file}
}

function bright {
  case $1 in
    (off)  xset dpms force off ;;
    (l|low)  s setpci -s 00:02.0 f4.b=35 ;;
    (m|med)  s setpci -s 00:02.0 f4.b=55 ;;
    (h|high) s setpci -s 00:02.0 f4.b=75 ;;
  esac
}

function taf {
  if (( $#@ >= 2 )); then
    tar -cf - ${argv[1,-2]} | tar -C ${argv[-1]} -xvf -
  else
    return 1
  fi
}

function pecho {
  while true; do
    mu12 ping1 $1 && stumpish echo "$1 estas supren"
    sleep 60
  done
}


function mky {
  local base_file="y.txt"

  if [[ -f "y.txt" ]]; then
    mv $base_file \
      ${base_file:r}-"$(print_date $base_file)".${base_file:e}
  fi

  if (( $#1 )); then
    egrep -o 'http(s)://(www.)youtube.com/watch\?v=.{11}' \
      "$1" | tac | uniq > $base_file
  fi
}

# doup pdflatex Zenjutsu.tex
function doup {
  $1 $2

  while :; do
    local stat_mod="`statmod $2`"
    sleep 1
    if [[ "`statmod $2`" -gt $stat_mod ]]; then
      $1 $2
    fi
  done
}

function locate_db {
  local db=

  if [[ -f /var/cache/locate/locatedb ]]; then
    db=/var/cache/locate/locatedb
  elif [[ -f ${HOME}/dat/locate/locatedb ]]; then
    db=${HOME}/dat/locate/locatedb
  else
    db=
  fi

  echo $db

}

function loc {
  local db="$(locate_db)"

  if [[ -n "$db" ]]; then
    locate -d "$db" -i "${@}"
  else
    echo "Error: locate db not found."
    return 1
  fi
}

function loh { loc "${@}" | egrep -i ${HOME} }

function lou {
  local db="$(locate_db)"

  if [[ -n "$db" ]]; then
    time s updatedb \
      --output="$db" \
      --prunepaths="/nix /tmp /var/tmp /media /run /home/ugarit /home/chrt /pub/mnt /b0 /mnt"
  else
    echo "Error: locate database not found."
    return 1
  fi
}

function a {
  local op=

  if (( ! $#1 )); then
    page $0
  else
    op=$1
    shift

    case $op in
      (get) s apt-get ${@} ;;
      (cache) apt-cache ${@} ;;
      (file) s apt-file ${@} ;;
      (att) s aptitude ${@} ;;
      (s)  $0 cache search ${@} ;;
      (fs) $0 file search ${@} ;;
      (h)  $0 cache show ${@} ;;
      (v)  $0 cache policy ${@} ;;
      (i)  $0 get install ${@} ;;
      (ri) $0 get install --reinstall ${@} ;;
      (r)  $0 get remove ${@} ;;
      (p)  $0 get purge ${@} ;;
      (d)  $0 cache depends ${@} ;;
      (rd) $0 cache rdepends ${@} ;;
      (o)  $0 get source ${@} ;;
      (u)  $0 get update ${@} ;;
      (uf) $0 file update ${@} ;;
      (fu) $0 get dist-upgrade ${@} ;;
      (su) $0 get upgrade ${@} ;;
      (c)  $0 get clean ${@} ;;
      (ac) $0 get autoclean ;;
      (f)  $0 get -f install ${@} ;;
      (y)  $0 get -q -y -d dist-upgrade ${@} ;;
      (ar) $0 get autoremove ${@} ;;
      (rm) s rm /var/lib/dpkg/lock /var/lib/apt/lists/lock ;;
      (l) dpkg -l ;;

      (*)  return 1 ;;
    esac
  fi
}

function om {
  if (( ! $#1 )); then
    page $0
  else
    op=$1
    shift

    case $op in
      (e) eval `opam config env` ;;

      (l) opam list ${@} ;;
      (i) opam install ${@} ;;
      (I) opam reinstall ${@} ;;
      (r) opam remove ${@} ;;
      (R) opam repository ${@} ;;
      (d) opam show ${@} ;;
      (s) opam search ${@} ;;
      (S) opam switch ${@} ;;
      (ud) opam update ${@} ;;
      (ug) opam ugrade ${@} ;;
      (u) opam update; opam upgrade ${@} ;;
      (c) opam config ${@} ;;

      (mli) ocamlfind ocamlc -package core -thread -i $1 > ${1:r}.mli ;;

      (C) find . \( -name _build -o -name configure -o -name 'setup.*' \
        -o -name Makefile -o -name '*.native' -o -name '*.byte' \) -print0 \
        | xargs -0 -I % rm -rf % ;;

      (O) oasis setup -setup-update dynamic ;;

      (co) map $0 C O ;;

      (*) opam $op ${@} ;;
    esac
  fi

}

function kvm-net {
  case $1 in
    (up)
      s vde_switch -tap tap0 -mod 660 -group kvm -daemon
      s ip addr add 10.0.2.1/24 dev tap0
      s ip link set dev tap0 up
      s sysctl -q -w net.ipv4.ip_forward=1
      s iptables -t nat -A POSTROUTING -s 10.0.2.0/24 -j MASQUERADE
      ;;
    (down)
      s iptables -t nat -D POSTROUTING -s 10.0.2.0/24 -j MASQUERADE
      s sysctl -q -w net.ipv4.ip_forward=0
      s ip link set dev tap0 down
      s ip link delete tap0
      s pkill -9 vde_switch
      s rm -f /run/vde.ctl/ctl
      ;;
  esac
}

function kvm-boot {
  s qemu-kvm -cpu host -m 2G -net nic,model=virtio -net vde \
    -soundhw all -vga qxl \
    -spice port=6900,addr=127.0.0.1,disable-ticketing \
    ${@}
}

function kvm-iso { kvm-boot -boot once=d -cdrom $1 ${argv[2,-1]} }
function kvm-display { spicy -p 6900 -h 127.0.0.1 ${@} }

function epk { if [[ -n "$1" ]]; then tar cJf - $1 | gpg -o $1.p -sec -; fi }
function dpk { gpg -o - -d $1 | tar -xJf -; }

function ug  { ugarit ${argv[1]} ${HOME}/etc/ugarit/ugarit.conf ${argv[2,-1]} }
function ugs { ug snapshot -v -c -a ${@} }
function uge { ug explore -v ${@} }
function ugx { ug extract -v ${@} }
function ugi { ug import -v ${@} }

function bootswatch {
  rm -f bootstrap.css bootstrap.min.css
  q http://bootswatch.com/$1/bootstrap.css http://www.bootswatch.com/$1/bootstrap.min.css
}

function v180 {
  local temp_file=out1.mp4
  mencoder -ovc lavc -oac pcm -vf rotate=1 -o $temp_file $1
  mencoder -ovc lavc -oac pcm -vf rotate=1 -o ${1:r}_rotated.mp4 $temp_file
  rm -f $temp_file
}

function lpath {
  local var=$LD_LIBRARY_PATH
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$1
  shift

  ${@}

  export LD_LIBRARY_PATH=$var
}

function lpathnix {
  lpath ${HOME}/.nix-profile/lib ${@}
}

# scz pi google.com
function scz { sc -d -m -S scz zsh -c "$*" }

# xl ${HOME}/Developer/shop/logs pi dns1
function xlv {
  if (( ! $#1 )); then
    return 1
  else
    local log=$1/$2.$3.log
    shift

    ${@} | tee -a $log
  fi

}

# sczxl . pi dns1
function sczxl { scz xl ${@} }
function spzxlph { sczxl . ph ${@} }

function pingp {
  if ping -c 1 -q $1 &> /dev/null; then
    return 0
  else
    return 1
  fi
}

function fur {
  wget -crpk -nH -np --tries=0 \
    -e robots=off \
    --cut-dirs=${2:-1} $1
}

function screenp {
  if screen -ls | grep -q $1; then
    return 0
  else
    return 1
  fi
}

function scs {
  if ! screenp $1; then
    screen -S $1
  else
    screen -D -R $1
  fi
}

function nrepl_port {
  local config_file=${HOME}/.lein/profiles.clj
  local default_port=9999
  local port=

  if [[ -f $config_file ]]; then
    port="$(sed -n -e 's/^.*:port\ \([0-9]*\).*$/\1/p' $config_file)"
  fi

  echo -n ${port:-$default_port}
}

function mklp {
  cat >! ${HOME}/.lein/profiles.clj < \
    <(cat ${HOME}/.lein/profiles.clj.skel | sed -e "s/_port_/${1:-$(nrepl_port)}/")
}

function mklr {
  local port=${1:-$(nrepl_port)}

  if ! free_port $port; then
    port=$(inc $port)
  fi

  mklp $port

  if [[ -f ${HOME}/.lein/profiles.clj ]]; then
    lein live :headless
  fi
}

function mklr! { cdx ${1:-$PWD} mklr $2 }
function lrs { scdm lr zsh -c lr }

function hn {
  nmap -sP 192.168.1.0/24 |
    egrep -o '^N.*for.*' |
    perl -pe 's/(N.*for\ )(.*)/\2/'
}

function meep {
  local text="Meep!"
  msg $text
  mutex espeak $text
}

function obuild {
  ocamlbuild -use-ocamlfind -classic-display -syntax camlp4o \
    -pkg core,core_extended,sexplib.syntax,comparelib.syntax,fieldslib.syntax,variantslib.syntax,bin_prot.syntax \
    -tag thread -tag debug -tag annot -tag bin_annot \
    -tag short_paths \
    -cflags \"-w A-4-33-40-41-42-43-34-44\" \
    -cflags -strict-sequence ${@}
}

function formulo {
  local name=formulo
  local dir=
  local out=`date +"%Y%m%d%H%M%S"`.png

  darwin_test && {
    dir=${HOME}/Desktop
  }

  linux_test && {
    dir=$(xdg-user-dir DESKTOP)/${name}
  }

  [[ ! -d $dir ]] && mkdir -p $dir

  (
    cd $dir

    if [[ ! -f ${name}.tex ]]; then
      cat > ${name}.tex <<EOF
\ifdefined\formula
\else
    \def\formula{E = m c^2}
\fi
\documentclass[border=2pt]{standalone}
\usepackage{amsmath}
\usepackage{varwidth}
\begin{document}
\begin{varwidth}{\linewidth}
\[ \formula \]
\end{varwidth}
\end{document}

EOF
    fi

    mute pdflatex "\def\formula{${@}}\input{${name}.tex}" && \
      mute convert -density 300 ${name}.pdf -quality 100 $out && \

      if [[ $! == 0 ]]; then
        for i in tex pdf log aux; do
          if [[ -f ${name}.$i ]]; then rm -f ${name}.$i; fi
        done

        echo ${dir}/${out}
      fi
  )
}

function merge {
  local temp=

  if (( ! $#2 )); then
    return 1
  else
    temp=`mktemp`
    cat $1 $2 >! $temp
    mv -f $temp .zhistory.new
  fi
}

function rmlock {
  case $1 in
    (emacs)
      mute rm -i -f ${HOME}/.emacs.d/desktop.lock
      ;;
    (apt)
      mute s rm /var/lib/dpkg/lock
      ;;
    (*)
      return 1
      ;;
  esac
}

function ssh-vagrant {
  local vm=$1
  local addr=$2
  shift 2

  ssh vagrant@$addr \
    -i ${HOME}/copa/$vm/.vagrant/machines/default/virtualbox/private_key \
    -o UserKnownHostsFile=/dev/null \
    -o StrictHostKeyChecking=no \
    -o PasswordAuthentication=no \
    -o IdentitiesOnly=yes \
    ${@}
}

function cr_tv {
  if ps aux | grep -q '/opt/teamviewer/tv_bin/teamviewerd -d'; then
    teamviewer ${@}
  else
    s teamviewer --daemon start
    teamviewer ${@}
  fi
}

function spkr {
  case $1 in
    (off) s modprobe -r pcspkr ;;
    (on) s modprobe pcspkr ;;
    (*) return 1 ;;
  esac
}

function pid {
  local i
  for i in /proc/<->/stat; do
    [[ "$(< $i)" = *\((${(j:|:)~@})\)* ]] && echo $i:h:t
  done
}

function cp_ico {
  if [[ -f "ico" ]]; then
    mvb ico
  else
    if [[ -d ${HOME}/Developer/bil/gxeneralaj/ikonoj/piktogramoj/"$1" ]]; then
      cp -a ${HOME}/Developer/bil/gxeneralaj/ikonoj/piktogramoj/"$1" ico
    else
      error "The requested favicon $1 does not exist."
    fi
  fi
}

function cp_eo_ico { cp_ico esperanto }

function cxu_enreta {
  local host=${1:-google.com}
  ping -c 1 "$host" 2> /dev/null | sed -n '/bytes from/s/.*=\(.*\)/\1/p'
}

function cxu_elreta { if cxu_enreta > /dev/null; then ne; else jes; fi }

function caps {
  if [[ $(tty) == /dev/tty[0-9]* ]]; then
    setleds -caps
  else
    xdotool key Caps_Lock
  fi
}

function fetch {
  for i in "${@}"; do
    aria2c "$i" || wget -t 0 -c "$i" || curl -O "$i" || error "Neniu el aria2c, wget, aŭ curl ekzistas.."
  done
}

function uj {
  local dir=/pub/apoj/jar

  lein uberjar
  find $dir -name 'emem-*SNAPSHOT-standalone.jar' -delete
  fx standalone.jar cp -vf % $dir

  (cdx $dir fx standalone.jar ln -sf % emem.jar)
}

function muziko {
  local dir=/pub/muzikoj

  ph --playlist <(l "$dir/$1"/**/*.mp3)
}

function kreu-sxablonon {
  local fonto=${HOME}/src/sxablonoj/

  case $1 in
    (Makefile) cp $fonto/Makefile .;;
    (*) warning "Ŝablono $1 ne ekzistas" ;;
  esac
}

function idr {
  NIX_CFLAGS_LINK="$NIX_CFLAGS_LINK -L $(nix-env --query --out-path gmp | awk '{print $2}')/lib" idris ${@}
}

function nas {
  local file=$1

  nasm -f elf64 -o ${file:r}.o ${file} && ld -o ${file:r} ${file:r}.o
  [[ -f ${file:r}.o ]] && rm -f ${file:r}.o
}

function heroku {
  docker run -it --rm -u $(id -u):$(id -g) -w "${HOME}" \
    -v /etc/passwd:/etc/passwd:ro \
    -v /etc/group:/etc/group:ro \
    -v /etc/localtime:/etc/localtime:ro \
    -v /home:/home \
    -v /tmp:/tmp \
    -v /run/user/$(id -u):/run/user/$(id -u) \
    --name heroku \
    johnnagro/heroku-toolbelt "${@}"
}

function cool {
  cr "coolc $1 && spim ${1:r}.s"
  [[ -f ${1:r}.s ]] && rm -f ${1:r}.s
}

function rmsample! {
  find . -maxdepth 1 -type f -iname '*sample*' -delete
}

function emake {
  if [[ -f Makefile ]]; then
    error "Jam ekzistas muntdosiero."
    return 1
  else
    if [[ -f ${HOME}/src/sxablonoj/Makefile${1} ]]; then
      =cp -f ${HOME}/src/sxablonoj/Makefile${1} Makefile
      shift
    else
      error "Ne ekzistas muntdosiero."
    fi

    =make ${@}
    rm -f Makefile
  fi
}

function btc {
  coproc bluetoothctl
  echo -e "power on\nagent on\ndefault-agent\nconnect 04:FE:A1:31:0B:7E\nexit" >&p
  sleep 10
  pacmd set-default-sink bluez_sink.04_FE_A1_31_0B_7E.a2dp_sink
}

function btd {
  coproc bluetoothctl
  echo -e "disconnect 04:FE:A1:31:0B:7E\nexit" >&p
}

function unzip! {
  unzip $1
  [[ $? == 0 ]] && rm -f $1
}

function emacs-daemon {
  if [[ -n "$(pg 'emacs.*--daemon')" || -n "$(pg 'emacs --smid')" ]]; then
    return 1
  else
    [[ -f "${HOME}/.emacs.d/desktop.lock" ]] && rm -f "${HOME}/.emacs.d/desktop.lock"
    emacs --no-desktop --daemon
  fi
}

function make% {
  local args=

  for arg in "${@}"; do
    args=("${(@s,/,)arg}")
    make -C $args[1]/$args[2] $args[3].html
  done
}

function make+ {
  local args=

  for file in $(git ls-files -m '*.md'); do
    make% "${file:r}"
  done
}

function make@ {
  for i in $(git lsm | xargs -I % basename %); do
    make ${i:r}.html
  done
}

function mk% {
  local args=

  for arg in "${@}"; do
    args=("${(@s,/,)arg}")
    mk -C $args[1]/$args[2] $args[3].html
  done
}

function mk@ {
  for i in $(git lsm | xargs -I % basename %); do
    mk ${i:r}.html
  done
}

function mk+ {
  local args=

  for file in $(git ls-files -m '*.md'); do
    mk% "${file:r}"
  done
}

function pdf+ {
  gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${argv[-1]} ${argv[1,-2]}
}

function %youtube-dl {
  local bin=$(which youtube-dl)
  local cmd=$1

  case "${cmd}" in
    (update)
      curl -L https://yt-dl.org/downloads/latest/youtube-dl -o "${bin}"
      chmod +x "${bin}"
      shift
      ;;
    (*)
      "${bin}" ${@}
      ;;
  esac
}

function stumpwm! {
  [[ -d ${HOME}/common-lisp ]] || mkdir ${HOME}/common-lisp

  if [[ -d "stumpwm" ]]; then
    cd stumpwm
    git pull --rebase origin master
  else
    git clone https://github.com/stumpwm/stumpwm
  fi
}

function cap {
  s tcpdump -s 65535 -i "$argv[1]" -w "$argv[2]" "$argv[3,-1]"
}

function clhs {
  for i in "${@}"; do
    emacsclient -nw -n -e "(hyperspec-lookup \"$i\")"
  done
}

function asm! {
  local file=$1

  nasm -f elf64 "${file}" && ld -o "${file:r}" "${file:r}".o && ./"${file:r}"
}

function cps {
  local dir=${HOME}/.screenshots
  cp "${dir}"/"$(ls -Art $dir | tail -n 1)" "$1"
}

function lf {
  for i in "${@}"; do
    s lsof -i \:${i}
  done
}

function snapshot {
  local dir=${2:-$PWD}
  tar cvzf "${dir}"/"${1}-snapshot-$(dt).tar.gz" "$1"
}

function sadd! {
  local dir=${HOME}/.ssh
  for key in ${dir}/*.pub; do
    echo ssh-add ${key:r}
  done
}

function pdf! {
  libreoffice --headless --convert-to pdf --outdir "$PWD" "$1"
}

function tar! {
  local date=$(date +"%Y%m%d%H%M%S")
  local tarball=${date}-${1}.tar.gz

  tar czf "${tarball}" "$1"
}

function dshell {
  local dir=

  darwin_test && {
    dir=${HOME}/Templates
  }

  linux_test && {
    dir=$(xdg-user-dir TEMPLATES)
  }

  d ${dir}/shell "${@}"
}

function btb {
  local mac=${1:-"20:18:12:00:04:5C"}

  docker run --rm -ti --privileged --net=host bluetooth_battery_level "${mac}"
}

function scandoc {
  if [[ $# -eq 1 ]]; then
    convert -density 150 "$1" -colorspace gray +noise Gaussian -rotate 0.5 -depth 2 "$(basename $1 .pdf)_scanned.pdf"
  fi
}

function mv% {
  if mute git ls-files --error-unmatch "$1"; then
    git mv "${@}"
  else
    mv "${@}"
  fi
}

function tars {
  local dest=(${(s/:/)argv[-1]})
  tar czf - ${argv[1,-2]} | ssh ${dest[1]} "tar -C ${dest[2]} -xvzf -"
}

function ql {
  if [[ $# -eq 1 ]]; then
    sbcl --eval "(ql:quickload :${@})"
  else
    sbcl --eval "(ql:quickload :$(basename $PWD))"
  fi
}

function eh {
  for file in "${@}"; do
    emacs "${file}" --batch -f org-html-export-to-html --kill
  done
}

function bd {
  for file in "${@}"; do
    resource=$(echo "$file" | sed 's|\.\([^/]*\)\.icloud$|\1|g')
    brctl download "${resource}"
  done
}

function icloud! {
  rm -rf ~/Library/Application\ Support/CloudDocs
  pkill bird
}

function cl@ {
  rm -rf ~/.cache/common-lisp
}

function brew@ {
  brew cleanup -s
  rm -rf $(brew --cache)
}

function nix@ {
  vix g -- -d
  vix s o
}

function trash@ {
  =rm -rf ~/.Trash/*
}

function doom@ {
  rm -f ~/.emacs.d

  ln -s "${HOME}/dat/doom" ~/.emacs.d
  ln -s "${HOME}/etc/doom" ~/.doom.d
}

function certinfo {
  curl --insecure -vvI "${@}" 2>&1 | awk 'BEGIN { cert=0 } /^\* SSL connection/ { cert=1 } /^\*/ { if (cert) print }'
}

function sed! {
  sed -i '' "${@}"
}

function km {
  local dir="${HOME}/etc/xmodmap"

  if [[ -f "${dir}/$1.xmap" ]]; then
    xmodmap "${dir}/$1.xmap"
  fi
}

function unz {
  if [[ $# -ge 1 ]]; then
    for file in "${@}"; do
      mvr "${file}"
      cd "${file:r}"
      7z x "${file}"
    done
  fi
}

function gh {
  gi "${@}" ${HOME}/.zhistory
}

#———————————————————————————————————————————————————————————————————————————————
# vterm

function vterm_printf {
  if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
    # Tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

function vterm_prompt_end {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

function vterm_cmd {
  local vterm_elisp
  vterm_elisp=""
  while [ $# -gt 0 ]; do
    vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
    shift
  done
  vterm_printf "51;E$vterm_elisp"
}

setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

function find_file {
  vterm_cmd find-file "$(realpath "${@:-.}")"
}

function say {
  vterm_cmd message "%s" "$*"
}

function dired {
  vterm_cmd dired "$(realpath "${@:-.}")"
}

function is_inside_emacs {
  if [[ "$INSIDE_EMACS" = "vterm" ]]; then
    return 0
  else
    return 1
  fi
}

is_inside_emacs && alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

function ec {
  if [[ $# -eq 0 ]]; then
    emacsclient -nc "$PWD"
  else
    emacsclient -nc "${@}"
  fi
}

function eceval {
  emacsclient --eval "${1}" &> /dev/null
}

function ecfind {
  eceval "(my-find-file-cwd \"${@}\")"
}

function e {
  local arg=

  if [[ $# -eq 0 ]]; then arg="${PWD}"; else arg="${1}"; fi

  if is_inside_emacs; then
    find_file "${arg}"
  else
    ecfind "${arg}"
  fi

  darwin_test && open -a Emacs.app
}

function dy {
  d ~/Downloads y "${@}"
}

function rb {
  for x in "${@}"; do
    true
  done
}

function conv {
  local out="$1"
  for file in ${argv[2,-1]}; do
    convert "${file}" "$(basename ${file} ${file:e})${out}"
  done
}

function dev {
  if [[ $# -ge 2 ]]; then
    vix develop ${HOME}/etc/dev#${1} -- --command ${argv[2,-1]}
  fi
}

#———————————————————————————————————————————————————————————————————————————————
# malgrandaĵoj

funs=(
  # sudo
  s "sudo"
  sue "s -Hiu"
  suc "s sh -c"
  root "sue root"

  # sistemprogramoj
  cryptsetup! "s =cryptsetup"
  losetup! "s =losetup"
  service! "s =service"
  journalctl! "s journalctl"
  pm-suspend! "s =pm-suspend"
  pm-hibernate! "s =pm-hibernate"
  blkid! "s =blkid"
  ipsec! "s =ipsec"
  hd "hexdump -C"
  rm+ "par 'rm! {}' :::"
  tup "watch -n 1 sudo netstat -tup"
  sen "watch -n 1 sudo sensors"
  tcpu 's =ps -eo pcpu,pid,user,args | sort -k 1 -r | head'
  tmem 's =ps -eo pmem,pid,user,args | sort -k 1 -r | head'

  # aplikaĵoj
  docker! "docker run -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix"
  diff "=diff -Naur"
  ub "mutex s unetbootin isofile=\"\$1\""
  xl "mutex xscreensaver-command -lock"
  say "mute espeak -a 300 -s 140 -p 0"
  iu "imgurbash2"
  ly "=lynx -accept_all_cookies"
  lump "ly -dump"
  lsrc "ly -source"
  dime "xdpyinfo | grep dimensions | awk '{print \$2}'"
  am "=alsamixer"
  tea "tee -a"
  xkd "setxkbmap dvorak"
  xku "setxkbmap us"
  fc! "fc-cache -fv"
  dm! "restart display-manager"
  xr! "xrdb ~/.Xresources"
  top! "s =top"
  htop! "s =htop"
  ncdu! "s =ncdu"
  pstree! "s =pstree"
  pelo! "pelo 1.1.1.1"
  pelo@ "pelo! | tee -a ${HOME}/dat/pelo/pelo.log"
  mtr! "s =mtr 8.8.8.8"
  iftop! "s =iftop -i wlp3s0"
  nm! "nm r"
  touchpad! "=touchpad disable"
  trackpoint! "=trackpoint 'TPPS/2 IBM TrackPoint'"
  spicy! "mutex spicy -h localhost -p 9999"
  zip! "zip -r"
  mi "mediainfo"
  vu "v ${HOME}/Developer/dok/utf-confusables/utf-confusables.txt"
  vc "v ${HOME}/etc/xcompose/sistemo.xcompose"
  iotop! "sudo =iotop -oaP"
  xb! "res xbindkeys"
  dp! "res devilspie2"
  nm "=nmcli"
  unison! "=unison -ui text"
  nosync "touch .nosync"

  # 0-j
  gitignore 'curl https://www.gitignore.io/api/$1; 0'
  ffmp3 'ffmpeg -y -i "$1" "${1:r}.mp3"; 0'
  ffmp4 'ffmpeg -strict -2 -i "$1" "${1:r}.mp4"; 0'
  digest 'md5sum <(print "${@}") | awk "{print $1}"; 0'
  tex@ 'for file (${@:-*.tex}) xelatex $file; 0'
  gif 'ffmpeg -i $1 -ss $2 -to $3 -r 30 -f image2pipe -vcodec ppm - | convert -loop 0 - $4; 0'
  e! 'e $(mktemp --dry-run -p ~/Downloads); 0'
  mhtml! '=emem -Fis -f -o ${2:-${1:r}.html} ${1}; 0'
  mhtml@ '=emem -Fis -o ${2:-${1:r}.html} ${1}; 0'
  mpdf! 'mhtml! $1 - | wkhtmltopdf -s Letter -q - ${2:-${1:r}.pdf}; 0'
  mpdf@ 'mhtml@ $1 - | wkhtmltopdf -s Letter -q - ${2:-${1:r}.pdf}; 0'
  mhtml 'mhtml@ ${@}; 0'
  mpdf 'mpdf@ ${@}; 0'
  mu@ 'mu ${1:r}.pdf; 0'
  lpr@ 'lpr ${1:r}.pdf; 0'
  lsofg 's lsof | grep "${@}"; 0'
  dpi 's dpkg -i $1; 0'
  dps 'dpkg -S =$1; 0'
  dpl 'dpkg -L $1; 0'
  ping1 'ping -c 1 "${@}"; 0'
  pdf2txt 'less "${@}" | cat; 0'
  statmod 'stat --printf "%Y\n" "$1"; 0'
  urlencode 'python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" "${@}"; 0'
  urldecode 'python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" "${@}"; 0'
  gistc 'gist ${@} | c; 0'
  lym 'emem -Rwo - ${@} | ly --stdin; 0'
  wdi 'wdiff -n ${@} | colordiff -u; 0'
  json2yaml 'ruby -ryaml -rjson -e "puts YAML.dump(JSON.parse(STDIN.read))" < $1 > $2; 0'
  yaml2json 'ruby -ryaml -rjson -e "puts JSON.pretty_generate(YAML.load(ARGF))" < $1 > $2; 0'
  tn 'tac | sed -n ${1}p; 0'
  bhelp 'bash -c "help ${@}" | less; 0'
  zr 'xzcat "$1" >! ~/.zhistory; 0'
  rml 'sed -i -e ${1}d $2; 0'
  rms 'rml $1 ${HOME}/.ssh/authorized_keys; 0'
  mine 's chown -R $USER ${@}; 0'
  res 'pkill $1; mutex $1; 0'
  calc 'echo ${@} | bc; 0'
  hnn 'mutex msg "$(hn)"; 0'
  twitch 'livestreamer $1 ${2:-medium}; 0'
  jj 'java -jar /pub/apoj/jar/$1.jar ${argv[2,-1]}; 0'
  mr 'find $1 -type f | sort -r | head -1; 0'
  rot13 'tr 'A-Za-z' 'N-ZA-Mn-za-m'; 0'
  ublocks 'sort $1 | sed -e '/^!/d' | uniq; 0'
  trim 'sed -i -e "$1,\$d" $2; 0'
  taf 'find $1 -maxdepth 1 -type d | egrep "(${2})\$" | tar -cf - -T - | tar -C $3 -xvf -; 0'
  tats 'find $1 -maxdepth 1 -type d | egrep "(${2})\$" | s tar -cvJf $3 -T -; 0'
  wmclass 'xprop | grep "^WM_CLASS" | sed -e "s/^.*\"\(.*\)\".*/\1/"; 0'
  pstty 'ps -t "$1" | sed 1d | awk "{print $1}"; 0'
  tex2png 'for i ("${@}") { tex2im -o ${i:r}.png -f png "$i" }; 0'
  mklast 'if [[ -f $1 ]]; then echo $1 >>! in.last; fi; 0'
  transfer 'curl --upload-file $1 https://transfer.sh/$(basename $1); 0'
  yv 'y $(curl -s $1 /e -io "/watch\?v=.{11}" /u /d -e "s|^|youtube.com|"); 0'
  xw '=wine ${HOME}/.wine/drive_c/${@}; 0'
  inc 'echo $(($1 + 1)); 0'

  # reto
  gateway "netstat -rn | grep UG | awk '{print \$2}'"
  trl "trickle -s"
  nmap! "s =nmap"
  nmap@ "nmap -T4 -A -v -Pn -p 1-65535"
  nmap+ "nmap -sS -sU -T4 -A -v -PE -PP -PS80,443 -PA3389 -PU40125 -PY -g 53 --script 'default or (discovery and safe)'"
  traceroute "s =traceroute"
  mtr! "s =mtr"
  netstat! "s =netstat"
  vncd "mutex =x11vnc -display :0 -shared -forever -ncache 10 -rfbauth ${HOME}/.vnc/passwd"
  progress! "s =progress -m"

  # xmonad
  dxmonad "d ~/.xmonad"

  # rakido
  pkg "raco pkg"
  frog "raco frog"
  livefrog "raco livefrog"

  # kloĵuro
  clo "lein deploy clojars"
  emem! "emem -Ffis"
  emem@ "emem -Fis"

  # haskelo
  ghci! "stack ghci"

  # lispo
  dcl "d ${HOME}/common-lisp"
  dasdf "dcl d asdf"
  duiop "dasdf d uiop"
  dstumpwm "dcl d stumpwm"
  dstumpo "dcl d stumpo"
  stumpo! "+ dstumpo make"
  dpm "dcl d pm"
  pm! "+ dpm make install"
  dwallpaper "dcl d wallpaper"
  wallpaper! "+ dwallpaper make install"

  # skimo
  scheme "rl =scheme"
  racket "rl =racket"
  guile "rl =guile"
  scsh "rl =scsh"
  scheme48 "rl =scheme48"
  chibi-scheme "rl =chibi-scheme"
  gxi "rl =gxi"
  csi "rl =csi"
  bigloo "rl =bigloo"
  tinyscheme "rl =tinyscheme"

  # apt
  apt "sudo =apt"

  # aliaj
  delpa "d ${HOME}/etc/emacs/var/elpa"
  dql "d ${HOME}/quicklisp/dists/quicklisp/software"

  # nix
  dnixos "d /etc/nixos"
  enixos "se /etc/nixos/configuration.nix"
  nixos! "sudo nixos-rebuild switch"
  nix! "vix rebuild -s"

  ddotfiles "d ${HOME}/etc/dotfiles"
  dttt "d ${HOME}/src/ttt/ebzzry.com"
  ettt "dttt e ."
  ttt! "+ dttt sc -dmS ttt python3 -m http.server 49153"

  # ssh
  @ "ssh"
  sagent "ssh-agent \$SHELL"
  sadd "ssh-add"

  # postgresql
  psql! "sue postgres psql"

  # dosierujoj
  dn "d ~/Documents"
  dr "d ~/Developer"
  dt "d ~/Desktop"
  dl "d ~/Downloads"
  dp "d ~/Pictures"
  dm "d ~/Movies"

  dqb "d ${HOME}/.qutebrowser"
  dqbl "d ${HOME}/.local/share/qutebrowser"
  dbin "d ${HOME}/bin"
  dvb "d ${HOME}/VirtualBox\ VMs"

  # rlwrap
  rl "rlwrap -s 1000000 -c -b \"(){}[].,=&^%$#@\\;|\""

  # make
  m "make"
  make! "make -B"

  # skim
  sk "=sk"
  sk@ "sk --regex"
  skt "sk-tmux"

  # xcompose
  vxsistemo "dktpxcompose v sistemo.xcompose"
  vxemogxioj "dktpxcompose v emogxioj.xcompose"
  vxceteraj "dktpxcompose v ceteraj.xcompose"

  # aliaj
  fceu "=fceux --keepratio 1 --fullscreen 1 --opengl --xscale 2 --yscale 2"
  tm "tmux"
  sc "screen"
  wget! "=wget -ct0"
  fos "fossil"
  dorg "d ${HOME}/org"
  dbeorg "d ${HOME}/beorg"
  dock! "killall Dock"
  bird! "killall bird"
  bt! "s killall bluetoothd"
  dicloud "d $HOME/Library/Mobile\ Documents/com~apple~CloudDocs"
  dworkspaces "d ${HOME}/etc/workspaces"

  # doom
  ddoom "d ${HOME}/.doom.d"
  demacs "d ${HOME}/.emacs.d"

  # alternativoj
  f "fd -HIi"
  g "rg --color auto -uuu"
  gi "g -i"
  v "less"
  va "v -A"

  # purigado
  cleanup@ 'cl@; brew@; nix@; 0'

  # zsh
  dzsh "d ${HOME}/etc/zsh"
); def_funs
