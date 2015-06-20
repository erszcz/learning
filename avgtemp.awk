#!/usr/bin/awk -f
$0 ~ /Core/ {
    sum += $3
    i += 1
    print $3
}
END {
    print sum
    print i
    print sum / i
}
