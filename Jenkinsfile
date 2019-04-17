#!groovy

pipeline {
  agent {
    docker {
      image 'geodynamics/calypso-buildenv-bionic:latest'
    }
  }

  options {
    timeout(time: 2, unit: 'HOURS')
  }

  stages {
    stage('Build') {
      steps {
        sh 'mkdir build'
        sh '''
          cd build
          cmake \
            ..
        '''
        sh '''
          cd build
          make
        '''
      }
    }

    stage('Test') {
      steps {
        sh '''
          cd build
          ctest --no-compress-output -T Test
        '''
      }
      post {
        always {
          xunit testTimeMargin: '3000',
            thresholdMode: 1,
            thresholds: [failed(), skipped()],
            tools: [CTest(pattern: 'build/Testing/**/*.xml')]
        }
      }
    }
  }
}
