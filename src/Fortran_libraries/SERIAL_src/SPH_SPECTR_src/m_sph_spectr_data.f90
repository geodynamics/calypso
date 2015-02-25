!>@file   m_sph_spectr_data.f90
!!@brief  module m_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Flag and parameters for spherical transform dnyamo model
!!
!!
!!@verbatim
!!      subroutine allocate_phys_rj_data
!!      subroutine allocate_reft_rj_data
!!
!!      subroutine deallocate_phys_rtp_data
!!      subroutine deallocate_phys_rj_data
!!      subroutine deallocate_reft_rj_data
!!
!!      subroutine check_rj_spectr_name
!!      subroutine check_rj_spectr_data(my_rank)
!!@endverbatim
!!
!!@n @param my_rank process ID
!
      module m_sph_spectr_data
!
      use m_precision
!
      implicit  none
!
!>      Number of fields for spectrum data  @f$ f(r,j) @f$
      integer (kind=kint) :: num_phys_rj
!>      Total number of components for spectrum data
!!      @f$ f(r,j) @f$
      integer (kind=kint) :: ntot_phys_rj
!
!>      Number of fields for visualization output
!!       @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_phys_rj_vis
!>      Total number of components  for visualization output
!!       @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: ntot_comp_rj_vis
!
!>      Integer flag for monitoring output
!!       for spectr data @f$ f(r,j) @f$
      integer (kind=kint), allocatable:: iflag_monitor_rj(:)
!
!>      Number of components for each field @f$ f(r,j) @f$
      integer (kind=kint), allocatable :: num_phys_comp_rj(:)
!>      End address of d_rj for each field @f$ f(r,j) @f$
      integer (kind=kint), allocatable :: istack_phys_comp_rj(:)
! 
!>      Field name for @f$ f(r,j) @f$
      character (len=kchara), allocatable :: phys_name_rj(:)
! 
!>      Field data @f$ f(r,\theta,\phi) @f$
      real (kind=kreal), allocatable :: d_rj(:,:)
!>      Spectr data @f$ f(r,j) @f$
!      real (kind=kreal), allocatable :: d_rtp(:,:)
!
!>      Number of fields of scalar fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_scalar_rtp
!>      Number of fields of vector fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_vector_rtp
!>      Number of fields of tensor fields @f$ f(r,\theta,\phi) @f$
      integer (kind=kint) :: num_tensor_rtp
!
!>    reference temerature spectr @f$ f(r,j) @f$
!!@verbatim
!!        reftemp_rj(kr,0) ... T_0
!!        reftemp_rj(kr,1) ... d T_0 / dr
!!@endverbatim
      real (kind=kreal), allocatable :: reftemp_rj(:,:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_rj_name
!
      allocate( phys_name_rj(num_phys_rj) )
      allocate( iflag_monitor_rj(num_phys_rj) )
      allocate( num_phys_comp_rj(num_phys_rj) )
      allocate( istack_phys_comp_rj(0:num_phys_rj) )
!
      if(num_phys_rj .gt. 0) then
        phys_name_rj = ''
        iflag_monitor_rj =    0
        num_phys_comp_rj =    0
      end if
      istack_phys_comp_rj = 0
!
      end subroutine allocate_phys_rj_name
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine allocate_phys_rj_data
!
      use m_spheric_parameter
!
      allocate( d_rj(nnod_rj,ntot_phys_rj) )
      if((nnod_rj*ntot_phys_rj) .gt. 0) d_rj = 0.0d0
!
      end subroutine allocate_phys_rj_data
!
!  --------------------------------------------------------------------
!
      subroutine allocate_reft_rj_data
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
      num = nidx_rj(1)
      allocate( reftemp_rj(num,0:1)   )
      reftemp_rj =  0.0d0
!
      end subroutine allocate_reft_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_phys_rj_data
!
      deallocate( phys_name_rj )
      deallocate( iflag_monitor_rj )
      deallocate( num_phys_comp_rj )
      deallocate( istack_phys_comp_rj )
      deallocate( d_rj )
!
      end subroutine deallocate_phys_rj_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_reft_rj_data
!
      deallocate( reftemp_rj )
!
      end subroutine deallocate_reft_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_name
!
      integer(kind = kint) :: i
!
!
      write(*,'(a,i16)') 'num_phys_rj ', num_phys_rj
      write(*,'(a)') 'number, component, stack, monitor_flag, name'
      do i = 1, num_phys_rj
        write(*,'(4i6,a2,a)') i, num_phys_comp_rj(i),                   &
     &                     istack_phys_comp_rj(i), iflag_monitor_rj(i), &
     &                     '  ', trim(phys_name_rj(i))
      end do
!
      end subroutine check_rj_spectr_name
!
!  --------------------------------------------------------------------
!
      subroutine check_rj_spectr_data(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
      write(50+my_rank,*) 'num_phys_rj',  num_phys_rj
      write(50+my_rank,*) 'ntot_phys_rj', ntot_phys_rj
      write(50+my_rank,*) 'num_phys_comp_rj', num_phys_comp_rj
      do inod = 1, num_phys_rj
        write(50+my_rank,*) phys_name_rj(inod)
      end do
      write(fmt_txt,'(a6,i3,a16)')                                      &
     &           '(3i16,', ntot_phys_rj, '(1pE25.15e3),a1)'
      do inod = 1, nnod_rj
        write(50+my_rank,fmt_txt) inod,                                 &
     &        idx_global_rj(inod,1:2), d_rj(inod,1:ntot_phys_rj)
      end do
!
!
      end subroutine check_rj_spectr_data
!
!  --------------------------------------------------------------------
!
      end module m_sph_spectr_data
