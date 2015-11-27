angular.module('totalTesting')
.controller('PracticeListController', ['Test',
  function(Test){
    var controller = this;
    controller.tests = [];
    controller.actionUrl = "#/practice/test/"
    getAll();

    function getAll() {
      Test.all().success(function(data) {
        controller.tests = data;
      });
    }
  }]);
