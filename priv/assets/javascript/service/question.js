angular.module('totalTesting').factory("Question", ["$http", function TestFactory($http) {
  var basePath = "/rest/v1/questions/";
    return {
      create: function(question) {
        return $http({method: "POST", url: basePath, data: question});
      },
      update: function(question) {
        return $http({method: "PUT", url: basePath + question.id, data: question});
      },
      deleteQuestion: function(questionId) {
        return $http({method: "Delete", url: basePath + questionId});
      },
      getByTest: function(testId) {
        return $http({method: "GET", url: "/rest/v1/tests/" + testId + "/questions"});
      },
      getRandom: function(number) {
        return $http({method: "GET", url: "/rest/v1/questions/random/" + number});
      }
    };
}]);
