#!/usr/bin/env -S zsh -f

#———————————————————————————————————————————————————————————————————————————————
# ĉefaferoj

function git {
  local git= self= op=

  if [[ -n "${BASH}" ]]; then
    git=$(which git)
    self=${FUNCNAME}
  elif [[ -n "${ZSH_NAME}" ]]; then
    git=$(whence -p git)
    self=$0
  else
    echo "Ve."
    return 1
  fi

  if [[ $# -eq 0 ]]; then
    if [[ -n "${BASH}" ]]; then
      type "${self}" | less
    elif [[ -n "${ZSH_NAME}" ]]; then
      which "${self}" | less
    else
      echo "Meh"
      return 1
    fi
  else
    op="$1"
    shift

    case "${op}" in
      (i) touch .gitignore; "${git}" init; "${self}" a.; "${self}" cim "${@}" ;;
      (i!) "${self}" i "[supro] pravalorizu" ;;

      (s) "${git}" status ;;
      (c) "${git}" clone "${@}" ;;
      (h) "${git}" show "${@}" ;;
      (mv) "${git}" mv "${@}" ;;
      (mv!) "${git}" mv -f "${@}" ;;
      (m) "${git}" merge "${@}" ;;
      (ta) "${git}" tag "${@}" ;;
      (bl) "${git}" blame "${@}" ;;

      (a) "${git}" add "${@}" ;;
      (au) "${self}" a -u ;;
      (a.) "${self}" a . ;;
      (aum) "${self}" au; "${self}" cim "${@}" ;;
      (a.m) "${self}" a.; "${self}" cim "${@}" ;;
      (a.x) "${self}" a.m "x" ;;
      (aux) "${self}" aum "x" ;;
      (auxx) "${self}" aux; "${self}" rs 2 ;;
      (au.x) "${self}" a.x; "${self}" rs 2 ;;
      (auxx!) "${self}" auxx; "${self}" oo! ;;

      (aul) "${self}" aum "$(git log -1 --pretty=%B)" ;;
      (a.l) "${self}" a.m "$(git log -1 --pretty=%B)" ;;

      (cl) "${git}" clean "${@}" ;;
      (cl!) "${self}" cl -fdx ;;

      (ci) "${git}" commit "${@}" ;;
      (cia) "${self}" ci --amend "${@}" ;;
      (cim) "${self}" ci --message "${@}" ;;

      (co) "${git}" checkout "${@}" ;;
      (com) "${self}" co main ;;
      (cot) "${self}" co trunk ;;
      (co!) "${self}" co --force "${@}" ;;
      (cob) "${self}" co -b "${@}" ;;

      (ls) "${git}" ls-files "${@}" ;;
      (lsm) "${self}" ls -m ;;
      (lsd) "${self}" ls -d ;;
      (lsdrm) "${self}" lsd | xargs "${git}" rm ;;

      (rt) "${git}" reset "${@}" ;;
      (rt!) "${self}" rt --hard "${@}" ;;
      (rv) "${git}" revert "${@}" ;;

      (g) "${git}" grep "${@}" ;;
      (gi) "${self}" g -i "${@}" ;;

      (f) "${git}" fetch "${@}" ;;
      (fa) "${self}" f --all "${@}" ;;

      (rm) "${git}" rm "${@}" ;;
      (rmr) "${self}" rm -r "${@}" ;;
      (rm!) "${self}" rm -rf "${@}" ;;

      (rb) "${git}" rebase "${@}" ;;
      (rbi) "${self}" rb --interactive "${@}" ;;
      (rbc) "${self}" rb --continue "${@}" ;;
      (rbs) "${self}" rb --skip "${@}" ;;
      (rba) "${self}" rb --abort "${@}" ;;
      (rbs) "${self}" rb --skip "${@}" ;;
      (rbi!) "${self}" rbi --root "${@}" ;;

      (ri) "${self}" rbi HEAD~"$1" ;;
      (rs) "${self}" rt --soft HEAD~"$1" && "${self}" cim "$(git log --format=%B --reverse HEAD..HEAD@{1} | head -1)" ;;

      (ph) "${git}" push "${@}" ;;
      (phu) "${self}" ph -u "${@}" ;;
      (ph!) "${self}" ph --force "${@}" ;;
      (pho) "${self}" phu origin "${@}" ;;
      (phoo) "${self}" phu origin "$(git brh)" ;;
      (phd) "${self}" ph --delete "${@}" ;;
      (phdo) "${self}" phd origin "$(git brh)" ;;
      (oo) "${self}" ph origin "$(git brh)" ;;
      (oo!) "${self}" ph! origin "$(git brh)" ;;

      (pl) "${git}" pull "${@}" ;;
      (pl!) "${self}" pl --force "${@}" ;;
      (plr) "${self}" pl --rebase "${@}" ;;
      (plro) "${self}" plr origin "${@}" ;;
      (plroo) "${self}" plr origin "$(git brh)" ;;
      (plru) "${self}" plr upstream "${@}" ;;
      (plruo) "${self}" plr upstream "$(git brh)" ;;

      (l) "${git}" log "${@}" ;;
      (l1) "${self}" l -1 --pretty=%B ;;
      (lo) "${self}" l --oneline ;;
      (lp) "${self}" l --patch ;;
      (lp1) "${self}" lp -1 ;;
      (lpw) "${self}" lp --word-diff=color ;;

      (br) "${git}" branch "${@}" ;;
      (bra) "${self}" br -a ;;
      (brm) "${self}" br -m "${@}" ;;
      (brmh) "${self}" brm "$(git brh)" ;;
      (brd) "${self}" br -d "${@}" ;;
      (brD) "${self}" br -D "${@}" ;;
      (brh) "${git}" rev-parse --abbrev-ref HEAD ;;

      (d) "${git}" diff "${@}" ;;
      (dc) "${git}" diff --cached "${@}" ;;
      (dh) "${self}" d HEAD ;;
      (dhw) "${self}" d --word-diff=color ;;

      (re) "${git}" remote "${@}" ;;
      (rea) "${self}" re add "${@}" ;;
      (reao) "${self}" rea origin "${@}" ;;
      (reau) "${self}" rea upstream "${@}" ;;
      (rer) "${self}" re remove "${@}" ;;
      (ren) "${self}" re rename "${@}" ;;
      (rero) "${self}" rer origin "${@}" ;;
      (reru) "${self}" rer upstream "${@}" ;;
      (res) "${self}" re show "${@}" ;;
      (reso) "${self}" res origin ;;
      (resu) "${self}" res upstream ;;

      (rl) "${git}" rev-list "${@}" ;;
      (rla) "${self}" rl --all "${@}" ;;
      (rl0) "${self}" rl --max-parents=0 HEAD ;;

      (cp) "${git}" cherry-pick "${@}" ;;
      (cpc) "${self}" cp --continue "${@}" ;;
      (cpa) "${self}" cp --abort "${@}" ;;

      (fb) FILTER_BRANCH_SQUELCH_WARNING=1 "${git}" filter-branch "${@}" ;;
      (fb!) "${self}" fb -f "${@}" ;;
      (fbt) "${self}" fb! --tree-filter "${@}" ;;
      (fbm) "${self}" fb! --msg-filter "${@}" ;;
      (fbi) "${self}" fb! --index-filter "${@}" ;;
      (fbe) "${self}" fb! --env-filter "${@}" ;;
      (fbc) "${self}" fb! --commit-filter "${@}" ;;

      (rp) "${git}" rev-parse "${@}" ;;
      (rph) "${self}" rp HEAD ;;

      (t) "${git}" stash "${@}" ;;
      (tp) "${self}" t pop "${@}" ;;

      (sbt) "${git}" subtree "${@}" ;;
      (sbta) "${self}" sbt add "${@}" ;;
      (sbtph) "${self}" sbt push "${@}" ;;
      (sbtpl) "${self}" sbt pull "${@}" ;;

      (sbm) "${git}" submodule "${@}" ;;
      (sbms) "${self}" sbm status "${@}" ;;
      (sbmy) "${self}" sbm summary "${@}" ;;
      (sbmu) "${self}" sbm update "${@}" ;;
      (sbma) "${self}" sbm add "${@}" ;;
      (sbmi) "${self}" sbm init "${@}" ;;

      (ref) "${git}" reflog "${@}" ;;

      (de) "${git}" describe "${@}" ;;
      (det) "${self}" de --tags "${@}" ;;

      (sw) "${git}" switch "${@}" ;;
      (swc) "${git}" switch -c "${@}" ;;

      (ro) "${git}" restore "${@}" ;;

      (wt) "${git}" worktree "${@}" ;;

      (sco) "${git}" sparse-checkout "${@}" ;;

      (bs) "${git}" bisect "${@}" ;;
      (bss) "${git}" bisect start "${@}" ;;
      (bsg) "${git}" bisect good "${@}" ;;
      (bsb) "${git}" bisect bad "${@}" ;;

      (zc) "${git}" log -G "${@}" --source --all --pretty=format:"%H" | cat ;;
      (zch) "${self}" zc "${@}" | head -n 1 | c ;;
      (zct) "${self}" zc "${@}" | tail -n 1 | c ;;
      (zd) "${git}" log -G "${@}" --source --all --pretty=format:"%ad" | cat ;;
      (zdh) "${self}" zd "${@}" | head -n 1 | c ;;
      (zdt) "${self}" zd "${@}" | tail -n 1 | c ;;

      (o) "${self}" oo! ;;
      (x) "${self}" auxx! ;;

      (cm) "${self}" fbm "${@}" HEAD ;;
      (ct) "${self}" fbt "${@}" --prune-empty --tag-name-filter cat -- --all ;;
      (cc) "${self}" fbc "if [[ \"\${GIT_AUTHOR_NAME}\" == \"$1\" && \"\${GIT_AUTHOR_EMAIL}\" == \"$2\" ]]; then
export GIT_AUTHOR_NAME=\"${3}\"; export GIT_AUTHOR_EMAIL=\"${4}\"; export GIT_COMMITTER_NAME=\"${3}\"; export GIT_COMMITTER_EMAIL=\"${4}\";
fi; git commit-tree \"\$@\"" ;;

      (*) "${git}" "${op}" "${@}" ;;
    esac
  fi
}
