function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the
%   cost of using theta as the parameter for linear regression to fit the
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%

h = X * theta;
diff = h - y;
J = diff .^ 2;
J = sum(J(:));

%% regularization (don't regularize theta(1))
thetaSS = sum(theta(2:end) .^ 2);

J = (J + (lambda * thetaSS)) / (2 * m);

%% derivative
diff = h - y;
grad = X' * diff;

%% regularization
grad = grad + (lambda * theta);
% don't regularize theta(1)
grad(1) = grad(1) - (lambda * theta(1));

grad = grad / m;

% =========================================================================

grad = grad(:);

end
