angular.module('totalTesting')
.controller('questionModalController', ['$scope', '$uibModalInstance', 'Question', 'params',
  function($scope, $uibModalInstance, Question, params){
    var controller = this;
    controller.question = {};
    controller.question.answers = [];
    if (params.question != undefined) {
       angular.copy(params.question, controller.question);
      controller.editing = true;
    }

    controller.finish = function () {
      $uibModalInstance.close();
    };

    controller.cancel = function () {
      $uibModalInstance.dismiss();
    };

    controller.saveQuestion = function() {
      setAnswers();
      if (validateQuestion()) {
        if(!controller.editing) {
          controller.question.test_id = params.testId;
          Question.create(controller.question).success(function(data) {
            cleanQuestion();
          });
        } else {
        Question.update(controller.question).success(function(data) {
          angular.copy(controller.question, params.question);
          cleanQuestion();
          controller.finish();
        });
        }
      }
    };

    controller.addAnswer = function() {
      controller.question.answers.push({description: '', is_correct: false});
    };

    controller.deleteAnswer = function(answer) {
      var index = controller.question.answers.indexOf(answer);
      controller.question.answers.splice(index, 1);
    };

    function setAnswers() {
      var answers = [];
      angular.forEach(controller.question.answers, function(value) {
        var description  = value.description.trim();
        var isCorrect  = value.is_correct;
        if (description.length > 0) {
          answers.push(value);
        }
      });
      controller.question.answers = answers;
    };

    function validateQuestion() {
      var result = false;
      if (controller.question.answers.length == 0) {
        alert("The question must have at least one answer!");
      } else {
        angular.forEach(controller.question.answers, function(answer) {
          if (answer.is_correct) {
            result = true;
          }
        });
        if (!result) {
          alert("The question must have at least one correct answer!");
        }
      }
      return result;
    };

    function cleanQuestion() {
      controller.question = {};
      controller.question.answers = [];
      $scope.questionForm.$setPristine();
    };
 }]);
