    X_m = X' * X;
    if det(X_m) == 0
        can_solve(i) = 0;
        continue
    end
    can_solve(i) = 1;
    
    % Construct and normalize the normal vector
    coeff = (X_m)^-1 * X' * data(:,i);
    
    s = 'this is a string'

    X_m = X' * X';
