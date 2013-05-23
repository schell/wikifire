angular.module('wikifire', ['wfrsrc']).
  config(function($routeProvider) {
    $routeProvider.
      when('/',    {controller:WelcomeCtrl, templateUrl:'/admin/welcome.html'}).
      when('/new', {controller:CreateCtrl,  templateUrl:'/admin/new.html'}).
      otherwise({redirectTo:'/'});
});

function WelcomeCtrl($scope, TemplateRoutes) {
}

function CreateCtrl($scope) {
}

function RoutesCtrl($scope, TemplateRoutes) {
  $scope.routes = TemplateRoutes.query(function() {
    console.log(arguments); 
  }); 
}

angular.module('wfrsrc', ['ngResource']).
  factory('TemplateRoutes', function($resource) {
    return $resource('/_templateNames');
});
