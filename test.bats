@test "modules" {
    for file in test/modules/*.f90; do
        echo $file && urclfortran -o testoutput.urcl -c $file
    done
}

@test "modules-fail" {
    for file in test/modules/error/*.f90; do
        echo $file
        run urclfortran -o testoutput.urcl -c $file
        [ "$status" -eq 255 ]
    done
}

@test "basic-functionality" {
    for file in test/basic_functionality/*.f90; do
        echo $file && urclfortran -o testoutput.urcl -c $file
    done
}

teardown() {
    rm *.fmod
    run rm testoutput.urcl
}