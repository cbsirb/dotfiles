ANSI_RED='\033[31m'
ANSI_RESET='\033[0m'

HASH="%C(yellow)%h%Creset"
RELATIVE_TIME="%Cgreen(%ar)%Creset"
AUTHOR="%C(bold blue)<%an>%Creset"
REFS="%C(bold red)%d%Creset"
SUBJECT="%s"

FORMAT="$HASH}$RELATIVE_TIME}$AUTHOR}$REFS $SUBJECT"

show_git_head() {
    pretty_git_log -1
    git show -p --pretty="tformat:"
}

pretty_git_log() {
    git log --graph --pretty="tformat:${FORMAT}" $* |
        # Replace (2 years ago) with (2 years)
        sed -Ee 's/(^[^<]*) ago\)/\1)/' |
        # Replace (2 years, 5 months) with (2 years)
        sed -Ee 's/(^[^<]*), [[:digit:]]+ .*months?\)/\1)/' |
        # Line columns up based on } delimiter
        column -s '}' -t |
        # Color merge commits specially
        sed -Ee "s/(Merge (branch|remote-tracking branch|pull request) .*$)/$(printf $ANSI_RED)\1$(printf $ANSI_RESET)/" |
        # Page only if we're asked to.
        if [ -n "$GIT_NO_PAGER" ]; then
            cat
        else
            # Page only if needed.
            less --quit-if-one-screen --no-init --RAW-CONTROL-CHARS --chop-long-lines
        fi
}