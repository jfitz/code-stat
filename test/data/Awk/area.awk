    # area.awk
    @namespace "area"

    BEGIN {
        pi = 3.14159  # namespaced "constant"
    }

    function circle(radius) {
        return pi*radius*radius
    }