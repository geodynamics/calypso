!>@file   t_energy_label_parameters.f90
!!@brief  module t_energy_label_parameters
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine init_energy_labels_base(ene_labels)
!!      subroutine alloc_energy_labels(ene_labels)
!!      subroutine dealloc_energy_labels(ene_labels)
!!        type(energy_label_param), intent(inout) :: ene_labels
!!
!!      subroutine set_sph_rms_labels                                   &
!!     &         (ene_labels, num_rms_comp, rms_name, labels)
!!        type(energy_label_param), intent(in) :: ene_labels
!!@endverbatim
!!
!
      module t_energy_label_parameters
!
      use m_precision
      use m_phys_labels
!
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
!
      implicit none
!
!
      integer(kind = kint), parameter :: n_fld_ene = 2
      character(len=kchara), parameter :: fld_lebel_4_ene(2)            &
     &                         = (/velocity%name, magnetic_field%name/)
      character(len=kchara), parameter :: ene_lebel_base(2)             &
     &                    = (/'K_ene     ', 'M_ene     '/)
!
!>      Structure of label for energies
      type energy_label_param
        integer(kind = kint) :: n_fld_4_ene
         character(len=kchara), allocatable :: field_name(:)
         character(len=kchara), allocatable :: label(:,:)
      end type energy_label_param
!
      private :: n_fld_ene, fld_lebel_4_ene, ene_lebel_base
      private :: set_sph_energy_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_energy_labels_base(ene_labels)
!
      use t_base_field_labels
      use add_direction_labels
!
      type(energy_label_param), intent(inout) :: ene_labels
!
      integer(kind = kint) :: i
!
      ene_labels%n_fld_4_ene = n_fld_ene
      call alloc_energy_labels(ene_labels)
!
      do i = 1, n_fld_ene
        ene_labels%field_name(i) = trim(fld_lebel_4_ene(i))
        call add_vector_power_sph_label(ene_lebel_base(i),              &
     &      ene_labels%label(1,i), ene_labels%label(2,i),               &
     &      ene_labels%label(3,i))
      end do
!
      end subroutine init_energy_labels_base
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_energy_labels(ene_labels)
!
      type(energy_label_param), intent(inout) :: ene_labels
!
      allocate(ene_labels%field_name(ene_labels%n_fld_4_ene))
      allocate(ene_labels%label(3,ene_labels%n_fld_4_ene))
!
      end subroutine alloc_energy_labels
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_energy_labels(ene_labels)
!
      type(energy_label_param), intent(inout) :: ene_labels
!
      deallocate(ene_labels%field_name, ene_labels%label)
!
      end subroutine dealloc_energy_labels
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels                                     &
     &         (ene_labels, num_rms_comp, rms_name, labels)
!
      use add_direction_labels
!
      type(energy_label_param), intent(in) :: ene_labels
      integer(kind = kint), intent(in) :: num_rms_comp
      character(len = kchara), intent(in) :: rms_name
!
      character(len = kchara), intent(inout) :: labels(num_rms_comp)
!
      logical :: flag
!
!
      if (num_rms_comp .eq. 1) then
        write(labels(1),'(a)')   trim(rms_name)
      else if (num_rms_comp .eq. 3) then
        flag = .FALSE.
        call set_sph_energy_labels                                      &
     &     (rms_name, ene_labels, labels(1), flag)
        if(flag) return
!
        call add_vector_power_sph_label(rms_name,                       &
     &      labels(1), labels(2), labels(3))
      else if (num_rms_comp .eq. 6) then
        call add_tensor_direction_label_rtp(rms_name,                   &
     &      labels(1), labels(2), labels(3),                            &
     &      labels(4), labels(5), labels(6))
      end if
!
      end subroutine set_sph_rms_labels
!
!  --------------------------------------------------------------------
!
      subroutine set_sph_energy_labels                                  &
     &         (rms_name, ene_labels, labels, flag)
!
      character(len = kchara), intent(in) :: rms_name
      type(energy_label_param), intent(in) :: ene_labels
!
      character(len = kchara), intent(inout) :: labels(3)
      logical, intent(inout) :: flag
!
      integer(kind = kint) :: i
!
!
      do i = 1, ene_labels%n_fld_4_ene
        if(rms_name .eq. ene_labels%field_name(i)) then
          write(labels(1),'(a)') trim(ene_labels%label(1,i))
          write(labels(2),'(a)') trim(ene_labels%label(2,i))
          write(labels(3),'(a)') trim(ene_labels%label(3,i))
          flag = .TRUE.
          exit
        end if
      end do
!
      end subroutine set_sph_energy_labels
!
! -----------------------------------------------------------------------
!
      end module t_energy_label_parameters
      
