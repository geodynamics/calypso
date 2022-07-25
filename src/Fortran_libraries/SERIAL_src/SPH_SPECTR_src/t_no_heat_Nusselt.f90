!>@file   t_no_heat_Nusselt.f90
!!@brief  module t_no_heat_Nusselt
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for Nusselt number
!!
!!@verbatim
!!      subroutine alloc_Nu_radial_reference(sph_rj, Nu_type)
!!      subroutine dealloc_Nu_radial_reference(Nu_type)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!
!!      subroutine set_ctl_params_no_heat_Nu                            &
!!     &         (source_name, Nusselt_file_prefix, rj_fld, Nu_type)
!!        character(len = kchara) :: source_name
!!        type(read_character_item), intent(in) :: Nusselt_file_prefix
!!        type(phys_data), intent(in) :: rj_fld
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!
!!      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,          &
!!     &          i_step, time, Nu_type)
!!
!!      subroutine open_read_no_heat_source_Nu(id_pick, Nu_type)
!!      subroutine read_no_heat_source_Nu                               &
!!     &         (id_pick, i_step, time, Nu_type, ierr)
!!@endverbatim
!
      module t_no_heat_Nusselt
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      File ID for Nusselt number IO
      integer(kind = kint), parameter :: id_Nusselt = 23
!
!>        Output flag for Nusselt number IO
      integer(kind = kint), parameter :: iflag_no_source_Nu = 1
!>        Output flag for Nusselt number IO
      integer(kind = kint), parameter :: iflag_source_Nu = 2
!
!>      Structure for Nusselt number data
      type nusselt_number_data
!>        Output flag for Nusselt number IO
        integer(kind = kint) :: iflag_Nusselt = 0
!>        File name for Nusselt number file
        character(len = kchara) :: Nusselt_file_name = 'Nusselt.dat'
!
!>        Radius at inner boundary
        real(kind = kreal) :: r_ICB_Nu
!>        Radius at outer boundary
        real(kind = kreal) :: r_CMB_Nu
!>        Nusselt number at inner boundary
        real(kind = kreal) :: Nu_ICB
!>        Nusselt number at outer boundary
        real(kind = kreal) :: Nu_CMB
!
        integer(kind = kint) :: nri_w_ctr
!>        diffusive profile and derivative
        real(kind = kreal), allocatable :: ref_global(:,:)
!>        local diffusive profile and derivative
        real(kind = kreal), allocatable :: ref_local(:,:)
      end type nusselt_number_data
!
      private :: id_Nusselt
      private :: open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_Nu_radial_reference(sph_rj, Nu_type)
! 
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      if(size(Nu_type%ref_global,1) .eq. sph_rj%nidx_rj(1)) return
      Nu_type%nri_w_ctr = sph_rj%nidx_rj(1)
!
      allocate(Nu_type%ref_global(0:Nu_type%nri_w_ctr,0:1))
      allocate(Nu_type%ref_local(0:Nu_type%nri_w_ctr,0:1))
!
!$omp parallel workshare
      Nu_type%ref_global(0:Nu_type%nri_w_ctr,0:1) = 0.0d0
      Nu_type%ref_local(0:Nu_type%nri_w_ctr,0:1) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_Nu_radial_reference
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_Nu_radial_reference(Nu_type)
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      if(allocated(Nu_type%ref_global) .eqv. .FALSE.) return
      deallocate(Nu_type%ref_global)
      deallocate(Nu_type%ref_local)
!
      end subroutine dealloc_Nu_radial_reference
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_no_heat_Nu                              &
     &         (source_name, Nusselt_file_prefix, rj_fld, Nu_type)
!
      use m_base_field_labels
      use m_grad_field_labels
      use t_phys_data
      use t_control_array_character
      use set_parallel_file_name
!
      character(len = kchara) :: source_name
      type(read_character_item), intent(in) :: Nusselt_file_prefix
      type(phys_data), intent(in) :: rj_fld
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      character(len = kchara) :: file_prfix
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      Nu_type%iflag_Nusselt = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. grad_temp%name) then
          Nu_type%iflag_Nusselt = iflag_no_source_Nu
          exit
        end if
      end do
!
      if(Nusselt_file_prefix%iflag .gt. 0) then
        Nu_type%iflag_Nusselt = iflag_no_source_Nu
        file_prfix = Nusselt_file_prefix%charavalue
        Nu_type%Nusselt_file_name = add_dat_extension(file_prfix)
      else
        Nu_type%iflag_Nusselt = 0
      end if
!
!    Turn Off Nusselt number if heat or composition source is there
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. source_name) then
!          Nu_type%iflag_Nusselt = iflag_source_Nu
          Nu_type%iflag_Nusselt = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      subroutine open_no_heat_source_Nu(Nu_type)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(nusselt_number_data), intent(in) :: Nu_type
!
!
      open(id_Nusselt, file = Nu_type%Nusselt_file_name,                &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_Nusselt, file = Nu_type%Nusselt_file_name,                                &
     &    form='formatted', status='replace')
!
!
      write(id_Nusselt,'(a)')    '# Inner_radius, Outer_radius'
      write(id_Nusselt,'(1p2e25.15e3)')                                 &
     &                         Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
!
      write(id_Nusselt,'(a)',advance='NO')                              &
     &    't_step    time    Nu_ICB    Nu_CMB'
      write(id_Nusselt,'(a)') ''
!
      end subroutine open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,            &
     &          i_step, time, Nu_type)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero, i_step
      real(kind = kreal), intent(in) :: time
      type(nusselt_number_data), intent(in) :: Nu_type
!
!
      if(Nu_type%iflag_Nusselt .eq. izero) return
      if(idx_rj_degree_zero .eq. izero) return
!
      call open_no_heat_source_Nu(Nu_type)
!
      write(id_Nusselt,'(i16,1p3e23.14e3)')                             &
     &       i_step, time, Nu_type%Nu_ICB, Nu_type%Nu_CMB
!
      close(id_Nusselt)
!
      end subroutine write_no_heat_source_Nu
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_read_no_heat_source_Nu(id_pick, Nu_type)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
!
!
      open(id_pick, file = Nu_type%Nusselt_file_name)
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
!
      read(id_pick,*) (tmpchara,i=1,4)
!
      end subroutine open_read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine read_no_heat_source_Nu                                 &
     &         (id_pick, i_step, time, Nu_type, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99)                                     &
     &        i_step, time, Nu_type%Nu_ICB, Nu_type%Nu_CMB
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module t_no_heat_Nusselt
