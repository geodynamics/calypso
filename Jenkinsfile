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
    stage('Build & Test') {
      steps {
        sh '''
          ./configure \
            --enable-fftw3 \
            --with-hdf5 \
            --with-blas \
            --with-zlib
        '''
        export OMPI_MCA_btl_vader_single_copy_mechanism=none
        sh 'make'
      }
    }
  }
}
