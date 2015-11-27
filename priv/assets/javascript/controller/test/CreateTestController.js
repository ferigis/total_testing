angular.module('totalTesting').controller('CreateTestController', ['$window', '$scope', 'Test', 'Question', '$uibModal',
  function($window, $scope, Test, Question, $uibModal){
    var controller = this;
    controller.test = {};

    controller.create = function() {
      Test.create(controller.test).success(function(data) {
        controller.test.id = data.test_id;
        openModal();
      });
    };

    function openModal () {
      var modalInstance = $uibModal.open({
        animation: true,
        templateUrl: 'addQuestionsModal.html',
        controller: 'questionModalController',
        controllerAs: 'ctrl',
        resolve: {
          params: function() {
            return {
              testId : controller.test.id
            }
          }
        }
      });

      modalInstance.result.then(function () {
        $window.location.href = '#/';
      }, function () {
        $window.location.href = '#/';
      });
    };
  }]);
