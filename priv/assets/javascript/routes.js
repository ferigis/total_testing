angular.module('totalTesting').config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'assets/templates/principal-buttons.html'
      }).
      when('/test/create', {
        templateUrl: 'assets/templates/test/create.html',
        controller: 'CreateTestController',
        controllerAs: 'createTestCtrl'
      }).
      when('/test/practice', {
        templateUrl: 'assets/templates/test/practice-buttons.html',
        controller: 'PracticeButtonsController',
        controllerAs: 'ctrl'
      }).
      when('/test/practice/list', {
        templateUrl: 'assets/templates/test/practice-list.html',
        controller: 'PracticeListController',
        controllerAs: 'ctrl'
      }).
      when('/practice/:practiceType/:testId', {
        templateUrl: 'assets/templates/test/practice.html',
        controller: 'PracticeController',
        controllerAs: 'ctrl'
      }).
      otherwise({
        redirectTo: '/'
    });
}]);
