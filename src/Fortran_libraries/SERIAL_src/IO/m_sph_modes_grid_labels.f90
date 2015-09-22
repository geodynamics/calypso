!>@file  m_sph_modes_grid_labels.f90
!!       module m_sph_modes_grid_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Comments for spherical harmonics indexing data
!!
!!@verbatim
!!      character(len=ilen_sph_para) function hd_sph_para()
!!      character(len=ilen_rtp_glbl) function hd_rtp_glbl()
!!      character(len=ilen_rtp_comm) function hd_rtp_comm()
!!
!!      character(len=ilen_rj_glbl) function hd_rj_glbl()
!!      character(len=ilen_rj_comm) function hd_rj_comm()
!!
!!      character(len=ilen_rlm_glbl) function hd_rlm_glbl()
!!
!!      character(len=ilen_sgmt) function hd_segment()
!!      character(len=ilen_trnc) function hd_trunc()
!!
!!      character(len=ilen_rgrd) function hd_rgrid()
!!      character(len=ilen_tgrd) function hd_tgrid()
!!      character(len=ilen_pgrd) function hd_pgrid()
!!      character(len=ilen_jmde) function hd_jmode()
!!
!!      character(len=ilen_grphd) function hd_grphd()
!!      character(len=ilen_ngrphd) function hd_ngrphd()
!!      character(len=ilen_rgrphd) function hd_rgrphd()
!!      character(len=ilen_tgrphd) function hd_tgrphd()
!!      character(len=ilen_pgrphd) function hd_pgrphd()
!!
!!      character(len=ilen_kgrphd) function hd_kgrphd()
!!      character(len=ilen_jgrphd) function hd_jgrphd()
!!@endverbatim
!
      module m_sph_modes_grid_labels
!
      use m_precision
!
!>      length of hd_sph_para
      integer(kind = kint), parameter                                   &
     &         :: ilen_sph_para = 1+24+14+34+27+1+6
!>      length of hd_rtp_glbl
      integer(kind = kint), parameter :: ilen_rtp_glbl = 1+40+21+30+1+5
!>      length of hd_rtp_comm
      integer(kind = kint), parameter :: ilen_rtp_comm = 1+39+1+3
!
!>      length of hd_rj_glbl
      integer(kind = kint), parameter :: ilen_rj_glbl = 1+40+21+32+1+5
!>      length of hd_rj_comm
      integer(kind = kint), parameter :: ilen_rj_comm = 1+41+1+3
!
!>      length of hd_rlm_glbl
      integer(kind = kint), parameter :: ilen_rlm_glbl = 1+40+21+36+1+5
!
!>      length of hd_segment
      integer(kind = kint), parameter :: ilen_sgmt = 1+11+31+1+4
!>      length of hd_trunc
      integer(kind = kint), parameter :: ilen_trnc = 1+22+42+1+4
!
!>      length of hd_rgrid
      integer(kind = kint), parameter :: ilen_rgrd = 1+33+13+1+4
!>      length of hd_tgrid
      integer(kind = kint), parameter :: ilen_tgrd = 1+33+17+1+4
!>      length of hd_pgrid
      integer(kind = kint), parameter :: ilen_pgrd = 1+46+15+1+4
!>      length of hd_jmode
      integer(kind = kint), parameter :: ilen_jmde = 1+33+41+1+4
!
!>      length of hd_grphd
      integer(kind = kint), parameter :: ilen_grphd = 1+12+1+3
!
!>      length of hd_ngrphd
      integer(kind = kint), parameter :: ilen_ngrphd = 1+13+1+3
!>      length of hd_rgrphd
      integer(kind = kint), parameter :: ilen_rgrphd = 1+15+1+3
!>      length of hd_tgrphd
      integer(kind = kint), parameter :: ilen_tgrphd = 1+19+1+3
!>      length of hd_pgrphd
      integer(kind = kint), parameter :: ilen_pgrphd = 1+14+1+3
!
!>      length of hd_pgrphd
      integer(kind = kint), parameter :: ilen_kgrphd = 1+25+1+3
!>      length of hd_pgrphd
      integer(kind = kint), parameter :: ilen_jgrphd = 1+28+1+3
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_sph_para) function hd_sph_para()
!
      hd_sph_para                                                       &
     &           = '!' // char(10)                                      &
     &          // '! 1.parallel information' // char(10)               &
     &          // '!    domain ID' // char(10)                         &
     &          // '!    number of domain for transfer' // char(10)     &
     &          // '!    domain ID for transfer' // char(10)            &
     &          // '!' // char(10)
!
      end function hd_sph_para
!
!------------------------------------------------------------------
!
      character(len=ilen_rtp_glbl) function hd_rtp_glbl()
!
      hd_rtp_glbl                                                       &
     &      = '!' // char(10)                                           &
     &     // '! number of stack number for each domain' // char(10)    &
     &     // '! local wavenumber ID' // char(10)                       &
     &     // '! global radial ID and grid ID' // char(10)              &
     &     // '!' // char(10)
!
      end function hd_rtp_glbl
!
!------------------------------------------------------------------
!
      character(len=ilen_rtp_comm) function hd_rtp_comm()
!
      hd_rtp_comm                                                       &
     &      = '!' // char(10)                                           &
     &     // '! communication table between grid data' // char(10)     &
     &     // '!' // char(10)
!
      end function hd_rtp_comm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_rj_glbl) function hd_rj_glbl()
!
      hd_rj_glbl                                                        &
     &      = '!' // char(10)                                           &
     &     // '! number of stack number for each domain' // char(10)    &
     &     // '! local wavenumber ID' // char(10)                       &
     &     // '! global radial ID and spectr ID' // char(10)            &
     &     // '!' // char(10)
!
      end function hd_rj_glbl
!
!------------------------------------------------------------------
!
      character(len=ilen_rj_comm) function hd_rj_comm()
!
      hd_rj_comm                                                        &
     &      = '!' // char(10)                                           &
     &     // '! communication table between spectr data' // char(10)   &
     &     // '!' // char(10)
!
      end function hd_rj_comm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_rlm_glbl) function hd_rlm_glbl()
!
      hd_rlm_glbl                                                       &
     &      = '!' // char(10)                                           &
     &     // '! number of stack number for each domain' // char(10)    &
     &     // '! local wavenumber ID' // char(10)                       &
     &     // '! global radial ID and wavenumber ID' // char(10)        &
     &     // '!' // char(10)
!
      end function hd_rlm_glbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_sgmt) function hd_segment()
!
      hd_segment = '!' // char(10)                                      &
     &          // '! Domain ID' // char(10)                            &
     &          // '! segment ID for each direction' // char(10)        &
     &          // '!' // char(10)
!
      end function hd_segment
!
!------------------------------------------------------------------
!
      character(len=ilen_trnc) function hd_trunc()
!
      hd_trunc                                                          &
     &      = '!' // char(10)                                           &
     &     // '! num. of global grids' // char(10)                      &
     &     // '! truncation level for spherical harmonics' // char(10)  &
     &     // '!' // char(10)
!
      end function hd_trunc
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_rgrd) function hd_rgrid()
!
      hd_rgrid =   '!' // char(10)                                      &
     &          // '! num. start and end global grids' // char(10)      &
     &          // '! r-direction' // char(10)                          &
     &          // '!' // char(10)
!
      end function hd_rgrid
!
!------------------------------------------------------------------
!
      character(len=ilen_tgrd) function hd_tgrid()
!
      hd_tgrid =   '!'  // char(10)                                     &
     &          // '! num. start and end global grids' // char(10)      &
     &          // '! theta direction' // char(10)                      &
     &          // '!' // char(10)
!
      end function hd_tgrid
!
!------------------------------------------------------------------
!
      character(len=ilen_pgrd) function hd_pgrid()
!
      hd_pgrid =   '!' // char(10)                                      &
     &          // '! num. of start and end global grids and modes'     &
     &          // char(10)                                             &
     &          // '! phi direction' // char(10)                        &
     &          // '!' // char(10)
!
      end function hd_pgrid
!
!------------------------------------------------------------------
!
      character(len=ilen_jmde) function hd_jmode()
!
      hd_jmode                                                          &
     &      = '!' // char(10)                                           &
     &     // '! num. start and end global modes' // char(10)           &
     &     // '! on sphere surface wuth degree and order' // char(10)   &
     &     // '!' // char(10)
!
      end function hd_jmode
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_grphd) function hd_grphd()
!
      hd_grphd =   '!' // char(10)                                      &
     &          // '! Group data' //  char(10)                          &
     &          // '!' // char(10)
!
      end function hd_grphd
!
!------------------------------------------------------------------
!
      character(len=ilen_ngrphd) function hd_ngrphd()
!
      hd_ngrphd =  '!' // char(10)                                      &
     &          // '! Node groups' //  char(10)                         &
     &          // '!' // char(10)
!
      end function hd_ngrphd
!
!------------------------------------------------------------------
!
      character(len=ilen_rgrphd) function hd_rgrphd()
!
      hd_rgrphd =  '!' // char(10)                                      &
     &          // '! radial groups' // char(10)                        &
     &          // '!' // char(10)
!
      end function hd_rgrphd
!
!------------------------------------------------------------------
!
      character(len=ilen_tgrphd) function hd_tgrphd()
!
      hd_tgrphd =  '!' // char(10)                                      &
     &          // '! meridional groups' // char(10)                    &
     &          // '!' // char(10)
!
      end function hd_tgrphd
!
!------------------------------------------------------------------
!
      character(len=ilen_pgrphd) function hd_pgrphd()
!
      hd_pgrphd =  '!' // char(10)                                      &
     &          // '! zonal groups'// char(10)                          &
     &          // '!' // char(10)
!
      end function hd_pgrphd
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_kgrphd) function hd_kgrphd()
!
      hd_kgrphd =  '!' // char(10)                                      &
     &          // '! radial groups in spectr' // char(10)              &
     &          // '!' // char(10)
!
      end function hd_kgrphd
!
!------------------------------------------------------------------
!
      character(len=ilen_jgrphd) function hd_jgrphd()
!
      hd_jgrphd =  '!' // char(10)                                      &
     &          // '! spehrical harmonics groups' // char(10)           &
     &          // '!' // char(10)
!
      end function hd_jgrphd
!
!------------------------------------------------------------------
!
      end module m_sph_modes_grid_labels
