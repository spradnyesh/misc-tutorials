function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters.

% Initialize some useful values
m = length(y); % number of training examples
n = size(X, 2); % number of features

% You need to return the following variables correctly
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta

h = sigmoid(theta' * X');

for i = 1:m
  first = (y(i) * log(h(i)));
  second = ((1 - y(i)) * log(1 - h(i)));
  J = J - (first + second);
endfor;

thetaSS = 0;
for j = 1:n
  thetaSS = thetaSS + (theta(j) ^ 2);
endfor
thetaSS = lambda * thetaSS / 2;
J = (J + thetaSS) / m;

diff = h' - y;
for j = 1:n
  for i = 1:m
    grad(j) = grad(j) + (diff(i) * X(i, j));
  endfor
  if (j > 1) % regularize
    grad(j) = grad(j) + (lambda * theta(j));
  endif
endfor

grad = grad / m;


% =============================================================

end
