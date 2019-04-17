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
        sh '''
          ./configure \
            --enable-fftw3 \
            --with-hdf5 \
            --with-blas
        '''
        sh 'make'
      }
    }

    stage('Test') {
      steps {
        sh 'make test'
      }
    }
  }
}
