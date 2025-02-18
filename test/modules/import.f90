module a
end module

module b
    use a
end

program c
    use a
    use b
end program c