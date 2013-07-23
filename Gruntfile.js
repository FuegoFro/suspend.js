/* jshint node: true */

module.exports = function (grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    jasmine: {
      dist: {
        src: 'dist/evaluator.compiled.js',
        options: {
          specs: 'evaluator.spec.js',
          vendor: [
            'lib/esprima/esprima.js',
            'https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js'
          ]
        }
      }
    },

    jshint: {
      gruntfile: {src: ['Gruntfile.js']},
      spec: {src: ['evaluator.spec.js']},
      src: {src: ['evaluator.js']},
      options: {
        jshintrc: '.jshintrc'
      }
    },

    coffee: {
      compile: {
        files: {
          'dist/evaluator.compiled.js': 'evaluator.coffee'
        }
      }
    },

    watch: {
      scripts: {
        files: ['*.coffee'],
        tasks: ['coffee'],
        options: {
          spawn: false
        }
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jasmine');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-coffee');
  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('default', ['coffee', 'jasmine', 'jshint']);
};
