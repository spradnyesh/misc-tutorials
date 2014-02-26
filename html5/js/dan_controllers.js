var demoApp = angular.module('demoApp', ['ngRoute']);

demoApp.config (function($routeProvider) {
    $routeProvider
        .when('/view1',
              {
                  controller: 'SimpleController',
                  templateUrl: 'partials/view1.html'
              })
        .when('/view2',
              {
                  controller: 'SimpleController',
                  templateUrl: 'partials/view2.html'
              })
        .otherwise(
            {
                redirectTo: '/view1'
            });
});

demoApp.controller('SimpleController', function($scope) {
    $scope.customers = [
        {
            name: 'John Smith',
            city: 'Pheonix'
        },
        {
            name: 'John Doe',
            city: 'New York'
        },
        {
            name: 'Jane Doe',
            city: 'San Francisco'
        }
    ];
    $scope.addCustomer = function() {
        $scope.customers.push(
            {
                name: $scope.newCustomer.name,
                city: $scope.newCustomer.city
            });
    };
});
