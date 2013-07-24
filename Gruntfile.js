/* jshint node: true */

module.exports = function (grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    jasmine: {
      dist: {
        src: 'dist/evaluator.compiled.js',
        options: {
          specs: 'dist/evaluator.spec.js',
          vendor: [
            'lib/esprima/esprima.js'
          ]
        }
      }
    },

    coffee: {
      compile: {
        files: {
          'dist/evaluator.compiled.js': 'evaluator.coffee',
          'dist/evaluator.spec.js': 'evaluator.spec.coffee'
        }
      }
    },

    coffeelint: {
      app: {
        files:{
          src: ['*.coffee']
        },
        options: {
          max_line_length: {
            value: 100
          }
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
  grunt.loadNpmTasks('grunt-contrib-coffee');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-coffeelint');

  grunt.registerTask('default', ['coffee', 'coffeelint', 'jasmine']);
};
