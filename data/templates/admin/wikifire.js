angular.module('wikifire', ['ngResource']).
  factory('TemplateRoutes', function($resource) {
    return $resource('/_templateNames', {}, {
      query: {
        method : 'GET',
        isArray: false
      }
    });
}).
  factory('Template', function($http, $location) {
    function Template(config) {
      config = config || {};

      this.name   = config.name   || '';
      this.source = config.source || '';
    }

    Template.prototype.get = function Template_get() {
      var self = this;
      $http({method : 'GET', url: '_'+this.name}).
        success(function(data, status, headers, config) {
          self.source = data;
      }).
        error(function(data, status, headers, config) {
          console.error('get template error',data,status,headers,config);
      });
    };

    Template.prototype.save = function Template_save(cb) {
      var self = this;
      return $http({
        method : 'POST',
        url: '/',
        headers: {'Content-Type' : 'application/x-www-form-urlencoded'},
        data : 'name='+encodeURIComponent(self.name)+'&source='+encodeURIComponent(self.source)
      }).
        success(function(data, status, headers, config) {
          console.log('post template success',data);
      }).
        error(function(data, status, headers, config) {
          console.error('post template error',data,status,headers,config);
      });
    };

    Template.prototype.view = function Template_view() {
      window.location.href = $location.protocol()+'://'+$location.host()+':'+$location.port()+this.name;
    };
    return Template;
}).
  config(function($routeProvider) {
    $routeProvider.
      when('/',                   {controller:WelcomeCtrl, templateUrl:'/admin/welcome.html'}).
      when('/new',                {controller:CreateCtrl,  templateUrl:'/admin/new.html'}).
      otherwise({redirectTo:'/'});

});

function WelcomeCtrl($scope, TemplateRoutes) {
}

function CreateCtrl($scope, $routeParams, Template) {
  var viewTemplateBtn = $('#view-template');
  var t = new Template($routeParams);

  if (t.name !== '') {
    t.get();
  }

  viewTemplateBtn.css('display','none');

  $scope.template = t;
  $scope.save = function CreateCtrl_save() {
    viewTemplateBtn.css('display','none');
    $scope.saveStatus = 'Saving...';
    t.save().
      success(function (data) {
        $scope.saveStatus = 'Done!';
        $scope.response = data;
        viewTemplateBtn.css('display','inline');
    }).
      error(function (data) {
        $scope.saveStatus = ':( Oh no! There was an error.';
    });
  };
}

function RoutesCtrl($scope, $location, TemplateRoutes) {
  $scope.routes = TemplateRoutes.query();
}

