#!/usr/bin/env fish

function desc --description 'Print the description of a Fish function.'

  # Check that $argv is not empty
  if test (count $argv) -eq 0;
    desc desc;
    printf 'Usage: desc { --all | function }\n'
      return 1
    end

    # Do we want to print *all* descriptions?
    if test $argv = '--all'
      for f in (functions | sed -E 's/(.*), /\1\n/g')
        desc $f;
      end;
      return 0

    else
      # Check that $argv is indeed a Fish function
      if not functions -q $argv
        printf '"%s" is not a function.\n' $argv
          return 1
        end

        # Check that the function has a description
        if not functions $argv | grep -q -e 'function '$argv' .*--description'
          printf 'The function "%s" has no description.\n' $argv
            return 2
          end
          
          # Print description
          printf '%s\t- %s\n' $argv (functions $argv | \
          grep 'function '$argv'.*--description' | sed -E "s|.*'(.*)'.*|\1|")
          end
        end
