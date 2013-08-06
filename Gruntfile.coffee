module.exports = (grunt) ->
  grunt.initConfig
    pkg: grunt.file.readJSON 'package.json'
    jasmine:
      dist:
        src: 'dist/suspend.js'
        options:
          specs: 'dist/suspend.spec.js'
          vendor: [
            'lib/esprima/esprima.js'
          ]

    coffee:
      compile:
        files:
          'dist/suspend.js': 'suspend.coffee'
          'dist/suspend.spec.js': 'suspend.spec.coffee'

    coffeelint:
      app:
        files:
          src: ['*.coffee']
        options:
          max_line_length:
            value: 100
          no_backticks:
            level: 'ignore'

    watch:
      scripts:
        files: ['*.coffee']
        tasks: ['coffee']
        options:
          spawn: false

  grunt.loadNpmTasks 'grunt-contrib-jasmine'
  grunt.loadNpmTasks 'grunt-contrib-coffee'
  grunt.loadNpmTasks 'grunt-contrib-watch'
  grunt.loadNpmTasks 'grunt-coffeelint'

  grunt.registerTask 'default', ['coffee', 'coffeelint', 'jasmine']
