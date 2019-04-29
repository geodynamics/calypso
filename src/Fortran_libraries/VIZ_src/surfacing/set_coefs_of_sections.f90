!
!      module set_coefs_of_sections
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine s_set_coefs_of_sections                              &
!!     &         (psf_c, id_section_method, const_psf, ierr)
!!      real(kind = kreal) function side_of_plane(const_psf, xx)
!!      subroutine cal_normal_of_plane(const_psf, xx, normal)
!
      module set_coefs_of_sections
!
      use m_precision
      use m_constants
!
      implicit  none
!
      character(len = kchara), parameter :: cflag_eq =  'equation'
      character(len = kchara), parameter :: cflag_pln = 'plane'
      character(len = kchara), parameter :: cflag_sph = 'sphere'
      character(len = kchara), parameter :: cflag_elp = 'ellipsoid'
      character(len = kchara), parameter :: cflag_hyp = 'hyperboloid'
      character(len = kchara), parameter :: cflag_prb = 'paraboloid'
      character(len = kchara), parameter :: cflag_grp = 'group'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_coefs_of_sections                                &
     &         (psf_c, id_section_method, const_psf, ierr)
!
      use m_error_IDs
      use t_control_data_4_psf
      use t_psf_patch_data
      use set_cross_section_coefs
!
      type(psf_ctl), intent(in) :: psf_c
!
      integer(kind = kint), intent(inout)  :: id_section_method
      real(kind = kreal), intent(inout) :: const_psf(10)
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      ierr = 0
      tmpchara = psf_c%section_method_ctl%charavalue
!
      if(cmp_no_case(tmpchara, cflag_eq)) then
        id_section_method = 1
        call set_coefs_4_psf(psf_c%psf_coefs_ctl%num,                   &
     &      psf_c%psf_coefs_ctl%c_tbl,  psf_c%psf_coefs_ctl%vect,       &
     &      const_psf(1) )
!
      else if(cmp_no_case(tmpchara, cflag_pln)) then
        id_section_method = 2
        call set_coefs_4_plane(psf_c, const_psf(1))
!
      else if(cmp_no_case(tmpchara, cflag_sph)) then
        id_section_method = 2
        call set_coefs_4_sphere(psf_c, const_psf(1))
!
      else if(cmp_no_case(tmpchara, cflag_elp)) then
        id_section_method = 3
        call set_coefs_4_ellipsode(psf_c, const_psf(1) )
!
      else if(cmp_no_case(tmpchara, cflag_hyp)) then
        id_section_method = 4
        call set_coefs_4_hyperboloide(psf_c, const_psf(1) )
!
      else if(cmp_no_case(tmpchara, cflag_prb)) then
        id_section_method = 5
        call set_coefs_4_parabolic(psf_c, const_psf(1) )
      else
        ierr = ierr_VIZ
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
      end subroutine s_set_coefs_of_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function side_of_plane(const_psf, xx)
!
      real(kind = kreal), intent(in) :: xx(3)
      real(kind = kreal), intent(in) :: const_psf(10)
!
!
      side_of_plane =  const_psf( 1) * (xx(1)*xx(1))                    &
     &               + const_psf( 2) * (xx(2)*xx(2))                    &
     &               + const_psf( 3) * (xx(3)*xx(3))                    &
     &               + const_psf( 4) * (xx(1)*xx(2))                    &
     &               + const_psf( 5) * (xx(2)*xx(3))                    &
     &               + const_psf( 6) * (xx(3)*xx(1))                    &
     &               + const_psf( 7) *  xx(1)                           &
     &               + const_psf( 8) *  xx(2)                           &
     &               + const_psf( 9) *  xx(3)                           &
     &               + const_psf(10)
!
      end function side_of_plane
!
!  ---------------------------------------------------------------------
!
      subroutine cal_normal_of_plane(const_psf, xx, normal)
!
      real(kind = kreal), intent(in) :: xx(3)
      real(kind = kreal), intent(in) :: const_psf(10)
!
      real(kind = kreal), intent(inout) :: normal(3)
!
!
      normal(1) =      const_psf( 1) *  xx(1) * two                     &
     &               + const_psf( 4) *  xx(2)                           &
     &               + const_psf( 6) *  xx(3)                           &
     &               + const_psf( 7) *  xx(1)
!
      normal(2) =      const_psf( 2) *  xx(2) * two                     &
     &               + const_psf( 4) *  xx(1)                           &
     &               + const_psf( 5) *  xx(3)                           &
     &               + const_psf( 8) *  xx(2)
!
      normal(3) =      const_psf( 3) *  xx(3) * two                     &
     &               + const_psf( 5) *  xx(2)                           &
     &               + const_psf( 6) *  xx(1)                           &
     &               + const_psf( 9) *  xx(3)
!
      end subroutine cal_normal_of_plane
!
!  ---------------------------------------------------------------------
!
      end module set_coefs_of_sections
