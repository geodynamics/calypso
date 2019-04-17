#!groovy

pipeline {
  agent {
    docker {
      image 'geodynamics/calypso-buildenv-bionic:latest'
      alwaysPull true
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
            -D CMAKE_Fortran_COMPILER='gfortran' \
            -D HDF5_INCLUDE_DIRS='/usr/include/hdf5/openmpi' \
            -D HDF5_LIBRARY_DIRS='/usr/lib/x86_64-linux-gnu/hdf5/openmpi' \
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
