#useful for showing all errors when testing
export ALCOTEST_SHOW_ERRORS=true

# execute tests
dune runtest --profile=release


# idea provare a i flatsub in flatadd ma con FLatImm con dentro il - o potrei creare un nuovo tipo come FlatImmMinus
# stessa cosa per i FlatDiv usando le frazioni in realta mi basterebbe chiamare un FlatImmFraction
