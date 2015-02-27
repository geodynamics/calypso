!cal_sph_node_addresses.f90
!      module cal_sph_node_addresses
!
!     Written by H. Matsui on March, 2012
!
!!
!!      subroutine reset_local_sph_node_constants
!!
!!      subroutine set_intnod_shell
!!      subroutine set_intnod_Spol
!!      subroutine set_intnod_Npole
!!      subroutine set_intnod_center
!!
!!      subroutine set_nnod_lc_shell(ip_r, ip_t)
!!      subroutine set_nnod_lc_Spole(intnod_input)
!!      subroutine set_nnod_lc_Npole(intnod_input)
!!      subroutine set_nnod_lc_center(intnod_input)
!!      subroutine set_nnod_lc_ctr_sph(nnod_sph_ct)
!!      subroutine set_nnod_lc_ctr_Np(intnod_input)
!!
!!      subroutine set_nnod_gl_shell
!!      subroutine set_nnod_gl_Spole
!!      subroutine set_nnod_gl_Npole
!!      subroutine set_nnod_gl_center
!!
!!      subroutine cal_sph_local_numnod(numnod, internal_node)
!!      subroutine check_local_sph_node_constants
!!
!!      integer(kind = kint) function sph_shell_node_id                 &
!!     &           (ip_r, ip_t, kr, lt, mp)
!!      integer(kind = kint) function sph_s_pole_node_i( kr)
!!      integer(kind = kint) function sph_n_pole_node_id(kr)
!!      integer(kind = kint) function sph_center_node_id()
!!      integer(kind = kint) function sph_ctr_shell_node_id             &
!!     &           (nnod_ct, lt, mp)
!!      integer(kind = kint) function sph_center_np_node_id()
!!
!!      integer(kind = kint) function global_sph_shell_node_id          &
!!                 (kr, lt, mp)
!!      integer(kind = kint) function global_sph_s_pole_node_id(kr)
!!      integer(kind = kint) function global_sph_n_pole_node_id(kr)
!!      integer(kind = kint) function global_sph_center_node_id()
!
      module cal_sph_node_addresses
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_sph_mesh_1d_connect
!
      implicit none
!
      integer(kind = kint) :: intnod_shell =   0
      integer(kind = kint) :: intnod_Spole =   0
      integer(kind = kint) :: intnod_Npole =   0
      integer(kind = kint) :: intnod_center =  0
!
      integer(kind = kint) :: nnod_lc_shell =   0
      integer(kind = kint) :: nnod_lc_Spole =   0
      integer(kind = kint) :: nnod_lc_Npole =   0
      integer(kind = kint) :: nnod_lc_center =  0
      integer(kind = kint) :: nnod_lc_ctr_sph = 0
      integer(kind = kint) :: nnod_lc_ctr_Np =  0
!
      integer(kind = kint) :: nnod_gl_shell =   0
      integer(kind = kint) :: nnod_gl_Spole =   0
      integer(kind = kint) :: nnod_gl_Npole =   0
      integer(kind = kint) :: nnod_gl_center =  0
!
      private :: intnod_shell,  intnod_center
      private :: intnod_Spole,  intnod_Npole
      private :: nnod_lc_shell, nnod_lc_center, nnod_lc_ctr_sph
      private :: nnod_lc_Spole, nnod_lc_Npole, nnod_lc_ctr_Np
      private :: nnod_gl_shell, nnod_gl_center
      private :: nnod_gl_Spole, nnod_gl_Npole
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reset_local_sph_node_constants
!
!
      intnod_shell =   0
      intnod_Spole =   0
      intnod_Npole =   0
      intnod_center =  0
!
      nnod_lc_shell =   0
      nnod_lc_Spole =   0
      nnod_lc_Npole =   0
      nnod_lc_center =  0
      nnod_lc_ctr_sph = 0
      nnod_lc_ctr_Np =  0
!
      nnod_gl_shell =   0
      nnod_gl_Spole =   0
      nnod_gl_Npole =   0
      nnod_gl_center =  0
!
      end subroutine reset_local_sph_node_constants
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_intnod_shell
!
!
      intnod_shell                                                      &
     &      = nidx_local_fem(1)*nidx_local_fem(2)*nidx_local_fem(3)
!
      end subroutine set_intnod_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_intnod_Spole
!
!
      intnod_Spole = nidx_local_fem(1)
!
      end subroutine set_intnod_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_intnod_Npole
!
!
      intnod_Npole = nidx_local_fem(1)
!
      end subroutine set_intnod_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_intnod_center
!
!
      intnod_center = ione
!
      end subroutine set_intnod_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_shell(ip_r, ip_t)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
!
      nnod_lc_shell = nnod_sph_r(ip_r)*nnod_sph_t(ip_t)                 &
     &               *nidx_global_fem(3)
!
      end subroutine set_nnod_lc_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_Spole(intnod_input)
!
      integer(kind = kint), intent(in) :: intnod_input
!
      nnod_lc_Spole = intnod_input
!
      end subroutine set_nnod_lc_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_Npole(intnod_input)
!
      integer(kind = kint), intent(in) :: intnod_input
!
      nnod_lc_Npole = intnod_input
!
      end subroutine set_nnod_lc_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_center(intnod_input)
!
      integer(kind = kint), intent(in) :: intnod_input
!
      nnod_lc_center = intnod_input
!
      end subroutine set_nnod_lc_center
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_ctr_sph(nnod_sph_ct)
!
      integer(kind = kint), intent(in) :: nnod_sph_ct
!
      nnod_lc_ctr_sph = nnod_sph_ct*nidx_global_fem(3)
!
      end subroutine set_nnod_lc_ctr_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_lc_ctr_Np(intnod_input)
!
      integer(kind = kint), intent(in) :: intnod_input
!
      nnod_lc_ctr_Np = intnod_input
!
      end subroutine set_nnod_lc_ctr_Np
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nnod_gl_shell
!
!
      nnod_gl_shell                                                     &
     &     = nidx_global_fem(1)*nidx_global_fem(2)*nidx_global_fem(3)
!
      end subroutine set_nnod_gl_shell
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_gl_Spole
!
!
      nnod_gl_Spole = nidx_global_fem(1)
!
      end subroutine set_nnod_gl_Spole
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_gl_Npole
!
!
      nnod_gl_Npole = nidx_global_fem(1)
!
      end subroutine set_nnod_gl_Npole
!
! -----------------------------------------------------------------------
!
      subroutine set_nnod_gl_center
!
!
      nnod_gl_center = ione
!
      end subroutine set_nnod_gl_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_local_numnod(numnod, internal_node)
!
      integer(kind = kint), intent(inout) :: numnod, internal_node
!
!
      numnod = nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole            &
     &        + nnod_lc_center + nnod_lc_ctr_sph + nnod_lc_ctr_Np
      internal_node = intnod_shell + intnod_Spole + intnod_Npole        &
     &               + intnod_center
!
      end subroutine cal_sph_local_numnod
!
! -----------------------------------------------------------------------
!
      subroutine check_local_sph_node_constants
!
!
      write(*,*) 'num_of_nodes  Internal, subdomain, global'
      write(*,*) 'shell', intnod_shell, nnod_lc_shell, nnod_gl_shell
      write(*,*) 'N_pole', intnod_Npole, nnod_lc_Npole, nnod_gl_Npole
      write(*,*) 'S_pole', intnod_Spole, nnod_lc_Spole, nnod_gl_Spole
      write(*,*) 'center',                                              &
     &           intnod_center, nnod_lc_center, nnod_gl_center
      write(*,*) 'center shell', nnod_lc_ctr_sph, nnod_lc_ctr_Np
!
      end subroutine check_local_sph_node_constants
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_shell_node_id                   &
     &                    (ip_r, ip_t, kr, lt, mp)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      sph_shell_node_id = kr + (lt-1)*nnod_sph_r(ip_r)                  &
     &                   + (mp-1)*nnod_sph_r(ip_r)*nnod_sph_t(ip_t)
!
      end function sph_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_s_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      sph_s_pole_node_id = kr + nnod_lc_shell
!
      end function sph_s_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_n_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      sph_n_pole_node_id = kr + nnod_lc_shell + nnod_lc_Spole
!
      end function sph_n_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_center_node_id()
!
!
      sph_center_node_id = 1 + nnod_lc_shell                            &
     &                    + nnod_lc_Spole + nnod_lc_Npole
!
      end function sph_center_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_ctr_shell_node_id               &
     &                   (nnod_ct, lt, mp)
!
      integer(kind = kint), intent(in) :: nnod_ct, lt, mp
!
!
      sph_ctr_shell_node_id = lt + (mp-1) * nnod_ct                     &
     &       + nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole            &
     &       + nnod_lc_center
!
      end function sph_ctr_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sph_center_np_node_id()
!
!
      sph_center_np_node_id                                             &
     &       = nnod_lc_shell + nnod_lc_Spole + nnod_lc_Npole            &
     &        + nnod_lc_center + nnod_lc_ctr_sph +  1
!
      end function sph_center_np_node_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function global_sph_shell_node_id            &
     &                    (kr, lt, mp)
!
      integer(kind = kint), intent(in) :: kr, lt, mp
!
!
      global_sph_shell_node_id = kr + (lt-1)*nidx_global_fem(1)         &
     &                   + (mp-1)*nidx_global_fem(1)*nidx_global_fem(2)
!
      end function global_sph_shell_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function global_sph_s_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      global_sph_s_pole_node_id = kr + nnod_gl_shell
!
      end function global_sph_s_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function global_sph_n_pole_node_id(kr)
!
      integer(kind = kint), intent(in) :: kr
!
!
      global_sph_n_pole_node_id = kr + nnod_gl_shell + nnod_gl_Spole
!
      end function global_sph_n_pole_node_id
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function global_sph_center_node_id()
!
!
      global_sph_center_node_id = 1 + nnod_gl_shell                     &
     &                           + nnod_gl_Spole + nnod_gl_Npole
!
      end function global_sph_center_node_id
!
! -----------------------------------------------------------------------
!
      end module cal_sph_node_addresses
