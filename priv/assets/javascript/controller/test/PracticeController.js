angular.module('totalTesting')
.controller('PracticeController', ['$window', 'Test', 'Question', '$routeParams', '$uibModal', '$route',
  function($window, Test, Question, $routeParams, $uibModal, $route){
    var controller = this;
    controller.test = {};
    controller.questions = [];
    controller.currentQuestion = {};
    controller.currentIndex = -1;
    controller.noQuestions = true;
    controller.testStarted = false;
    controller.lastQuestion = false;
    controller.firstQuestion = false;
    controller.editing = {};
    controller.editing.test = false;

    init();

    controller.start = function () {
      controller.testStarted = true;
      controller.next();
    };

    controller.prev = function () {
      if (controller.currentIndex > 0) {
        controller.currentIndex--;
        controller.currentQuestion = controller.questions[controller.currentIndex];
        controller.lastQuestion = false;
        checkFirstQuestion();
        checkLastQuestion();
      }
    };

    controller.next = function () {
      if (controller.currentIndex < controller.questions.length - 1) {
        controller.currentIndex++;
        controller.currentQuestion = controller.questions[controller.currentIndex];
        checkFirstQuestion();
        checkLastQuestion();
      }
    };

    controller.checkCorrect = function (answer) {
      if (answer.is_correct) {
        answer.class = "list-group-item-success";
      } else {
        answer.class = "list-group-item-danger";
      }
    };

    controller.cancelTest = function () {
      controller.test.editedName = controller.test.name;
      controller.editing.test = false;
    };

    controller.saveTest = function () {
      var test = {id: controller.test.id, name: controller.test.editedName};
      Test.update(test).success(function(data) {
        controller.test = test;
        controller.test.editedName = controller.test.name;
        controller.editing.test = false;
      });
    };

    controller.deleteTest = function() {
      openDeleteTestConfirmation("Do you want to delete the Test?", function() {
        Test.deleteTest(controller.test.id).success(function(data) {
          $window.location.href = '#/';
        });
      });
    };

    controller.deleteQuestion = function() {
      openDeleteTestConfirmation("Do you want to delete the Question?", function() {
        Question.deleteQuestion(controller.currentQuestion.id).success(function(data) {
          $route.reload();
        });
      });
    };

    controller.addQuestions = function () {
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
        $route.reload();
      }, function () {
        $route.reload();
      });
    };

    controller.editQuestion = function () {
      var modalInstance = $uibModal.open({
        animation: true,
        templateUrl: 'addQuestionsModal.html',
        controller: 'questionModalController',
        controllerAs: 'ctrl',
        resolve: {
          params: function() {
            return {
              testId : controller.test.id,
              question: controller.currentQuestion
            }
          }
        }
      });

      modalInstance.result.then(function () {

      });
    };


    
    function openDeleteTestConfirmation(msg, callback) {
      var modalInstance = $uibModal.open({
        animation: true,
        templateUrl: 'confirmationModal.html',
        controller: 'confirmModalController',
        controllerAs: 'ctrl',
        resolve: {
          msg: function() {
            return msg;
          }
        }
      });

      modalInstance.result.then(function () {
        callback();
      });
    };

    function init() {
      switch ($routeParams.practiceType) {
        case "test":
          getTest();
          break;
        case "random":
          getRandom();
          break;
      }
    };

    function getRandom() {
      Question.getRandom($routeParams.testId).success(function(data) {
        controller.test = {name: "Random Questions"};
        controller.randomMode = true;
        if (data.length > 0) {
          controller.questions = data;
          controller.noQuestions = false;
        };
      });
    }

    function getTest() {
      Test.get($routeParams.testId).success(function(data) {
        controller.test = data;
        controller.test.editedName = controller.test.name;
        if (controller.test.questions_number > 0) {
          getQuestions($routeParams.testId);
          controller.noQuestions = false;
        }
      });
    };

    function getQuestions(testId) {
      Question.getByTest(testId).success(function(data) {
        controller.questions = data;
      });
    };

    function checkFirstQuestion() {
      if (controller.currentIndex == 0) {
        controller.firstQuestion = true;
      } else {
        controller.firstQuestion = false;
      }
    };

    function checkLastQuestion() {
      if (controller.currentIndex == controller.questions.length - 1) {
        controller.lastQuestion = true;
      } else {
        controller.lastQuestion = false;
      }
    };
  }]);

angular.module('totalTesting')
.controller('confirmModalController', ['$uibModalInstance', 'msg',
  function($uibModalInstance, msg){
    var controller = this;
    controller.msg = msg;

    controller.accept = function () {
      $uibModalInstance.close();
    };

    controller.cancel = function () {
      $uibModalInstance.dismiss();
    };
 }]);
