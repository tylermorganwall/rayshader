#!/usr/bin/env bash
set -eo pipefail

read -ra fc <<< "$(R CMD config FC)"
[[ ${fc[0]##*/} == flang* ]] || exit 0

makevars=$(Rscript -e 'cat(tools::makevars_user())')
makevars=${makevars:-${R_MAKEVARS_USER:-$HOME/.R/Makevars}}
mkdir -p "$(dirname "$makevars")"

# igraph adds F_VISIBILITY to the flags for its ARPACK sources.
probe="$RUNNER_TEMP/visibility.f90"
echo 'end' > "$probe"
if ! "${fc[@]}" -fvisibility=hidden -c "$probe" -o /dev/null 2>/dev/null; then
  "${fc[@]}" -c "$probe" -o /dev/null
  echo "Disabling unsupported Fortran visibility flag for ${fc[*]}"
  printf '\nF_VISIBILITY =\n' >> "$makevars"
fi

# Newer Flang versions ship a target-specific runtime. R-hub's Makeconf
# can still list only the older FortranDecimal library.
if resource_dir=$("${fc[@]}" -print-resource-dir 2>/dev/null) &&
   target=$("${fc[@]}" -print-target-triple 2>/dev/null); then
  runtime="$resource_dir/lib/$target/libflang_rt.runtime.a"
  if [[ -f $runtime && $(R CMD config FLIBS) != *flang_rt.runtime* ]]; then
    echo "Adding $runtime to R's Fortran link flags"
    printf '\nFLIBS := %s $(FLIBS)\n' "$runtime" >> "$makevars"
  fi
fi
