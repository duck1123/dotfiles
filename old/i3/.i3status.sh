i3status --config ~/.i3/i3status.conf | \
    while :
    do
        read line
        toggl=$(toggl now)
        playing=$(ncmpcpp -h dev.jiksnu.com --now-playing "%a - %t")
        # playing=$(ncmpcpp -h localhost --now-playing  "%a - %t")

        echo "$playing | $toggl | $line" || exit 1
    done
