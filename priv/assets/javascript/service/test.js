angular.module('totalTesting').factory("Test", ["$http", function TestFactory($http) {
  var basePath = "/rest/v1/tests/";
    return {
      create: function(test) {
        return $http({method: "POST", url: basePath, data: test});
      },
      update: function(test) {
        return $http({method: "PUT", url: basePath + test.id, data: test});
      },
      deleteTest: function(testId) {
        return $http({method: "DELETE", url: basePath + testId});
      },
      get: function(testId) {
        return $http({method: "GET", url: basePath + testId});
      },
      all: function() {
        return $http({method: "GET", url: basePath});
      }
    }
}]);
