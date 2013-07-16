/* jshint node: true */

module.exports = function (grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    jasmine: {
      dist: {
        src: 'evaluator.js',
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
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jasmine');
  grunt.loadNpmTasks('grunt-contrib-jshint');

  grunt.registerTask('default', ['jasmine', 'jshint']);
};
