#!/bin/bash
root_file="$(mktemp)"
git_root="$(git rev-parse --show-toplevel)"
jq '[.customManagers | .[] | select(.customType == "regex")]' < "$git_root/renovate.json" > "$root_file"
count_item="$(jq 'length' < "$root_file")"
all_files_temp="$(mktemp)"
find "$(git rev-parse --show-toplevel)" -type f -print0 > "$all_files_temp"
# echo "pointer: $all_files_temp"
# shellcheck disable=SC2016
strip_git_root_query='[inputs | select(length>0)]'\
' | map(. | sub(("^" + '\
'($root | reduce ("\\\\", "\\*", "\\^", "\\?", "\\+", "\\.", "\\!", "\\{", "\\}", "\\[", "\\]", "\\$", "\\|", "\\(", "\\)" ) as $c '\
'(.; gsub($c; $c)) ) + "/"); "" )) | .[]'
all_files_matcher_temp="$(mktemp)"
tr '\0' '\n' < "$all_files_temp" | jq --arg root "$git_root" -Rr "$strip_git_root_query" > "$all_files_matcher_temp"
item_file="$(mktemp)"
err_msg_header=$'\e[48;5;124m\e[38;5;15m[ERROR]\e[m'
errors=""
LF=$'\n'
# cat "$root_file"
for ((of_item=0; of_item < "$count_item"; of_item++)); do
grep_temp="$(mktemp)"
jq --arg of_item "$of_item" '.[$of_item | tonumber]' < "$root_file" > "$item_file"
# echo "item:"
# cat "$item_file"
# echo "----"
count_file_match="$(jq -r '.managerFilePatterns | length' < "$item_file")"
for ((of_file_match=0; of_file_match < "$count_file_match"; of_file_match++)); do
  current_regex="$(jq -r --arg i "$of_file_match" '.managerFilePatterns[$i | tonumber]' < "$item_file" | awk '{print substr($0, 2, length($0)-2)}')"
  # echo "regex: $current_regex"
  grep -aE "$current_regex" < "$all_files_matcher_temp" >> "$grep_temp" || {
    colored_regex=$'\e[38;5;214m'"${current_regex}"$'\e[m'
    errors="${errors}${err_msg_header} .customManagers[${of_item}].managerFilePatterns[${of_file_match}] (${colored_regex}) does not match to any files.$LF"
  }
  done

  if [ -n "$errors" ]; then break; fi
  count_string_match="$(jq -r '.matchStrings | length' < "$item_file")"
  for ((of_string_match=0; of_string_match < "$count_string_match"; of_string_match++)); do
    current_regex="$(jq -r --arg i "$of_string_match" '.matchStrings[$i | tonumber]' < "$item_file")"
    xargs grep -Pzo "$current_regex" < "$grep_temp" >/dev/null || {
      colored_regex=$'\e[38;5;214m'"${current_regex}"$'\e[m'
      errors="${errors}${err_msg_header} .customManagers[${of_item}].matchStrings[${of_string_match}] (${colored_regex}) does not match to following files:$LF$(sed -E 's/^/- /' "$grep_temp")$LF"
    }
  done
  rm "$grep_temp"
done

if [ -n "$errors" ]; then
  echo "$errors" >&2
  exit 1
fi
