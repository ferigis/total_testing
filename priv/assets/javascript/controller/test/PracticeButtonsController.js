angular.module('totalTesting')
.controller('PracticeButtonsController', ['$window', '$uibModal',
  function($window, $uibModal){
    var controller = this;

    controller.open = function (size) {

      var modalInstance = $uibModal.open({
        animation: true,
        templateUrl: 'randomQuestionsModal.html',
        controller: 'PracticeModalController',
        controllerAs: 'ctrl'
      });

      modalInstance.result.then(function (numberOfQuestions) {
        if (!numberOfQuestions) numberOfQuestions = 1;
        $window.location.href = '#/practice/random/' + numberOfQuestions;
      });
    };

  }]);

angular.module('totalTesting')
.controller('PracticeModalController', ['$uibModalInstance',
  function($uibModalInstance){
    var controller = this;
    controller.numberOfQuestions = 1;

    controller.ok = function () {
      $uibModalInstance.close(controller.numberOfQuestions);
    };

    controller.cancel = function () {
      $uibModalInstance.dismiss();
    };
 }]);
