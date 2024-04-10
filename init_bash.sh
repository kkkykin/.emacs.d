myprompt () {
    printf "\e]7;file://%s%s\e\\" "$HOSTNAME" "$PWD"
}
PROMPT_COMMAND=myprompt

export GIT_PAGER=''
