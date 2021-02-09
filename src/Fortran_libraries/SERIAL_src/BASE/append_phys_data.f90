!>@file   append_phys_data.f90
!!        module append_phys_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!!      Modified March, 2020
!!
!> @brief Copy and append field data
!!
!!@verbatim
!!      subroutine append_field_name_list(field_name, numdir,           &
!!     &          flag_viz, flag_monitor, iorder_eletype, fld)
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine copy_field_name(org_fld, new_fld)
!!      subroutine copy_field_data(org_fld, new_fld)
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!
!!      integer(kind = kint) function compare_field_data(fld1, fld2)
!!       type(phys_data), intent(in) :: fld1, fld2
!!@endverbatim
!
      module append_phys_data
!
      use m_precision
      use m_machine_parameter
      use t_phys_data
!
      implicit  none
! 
      private :: add_field_name_list_4_viz, add_field_name_list_noviz
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine append_field_name_list(field_name, numdir,             &
     &          flag_viz, flag_monitor, iorder_eletype, fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: numdir
      logical, intent(in) :: flag_viz, flag_monitor
      integer(kind = kint), intent(in) :: iorder_eletype
!
      type(phys_data), intent(inout) :: fld
!
      type(phys_data) :: tmp_fld
!
!
      call copy_field_name(fld, tmp_fld)
      call dealloc_phys_name(fld)
!
      fld%num_phys =      fld%num_phys + 1
      fld%ntot_phys =     fld%ntot_phys + numdir
!
      if(flag_viz) then
        fld%num_phys_viz =  fld%num_phys_viz + 1
        fld%ntot_phys_viz = fld%ntot_phys_viz + numdir
!
        call alloc_phys_name(fld)
        call add_field_name_list_4_viz                                  &
     &     (field_name, numdir, flag_monitor, iorder_eletype,           &
     &      tmp_fld, fld)
      else
        call alloc_phys_name(fld)
        call add_field_name_list_noviz                                  &
     &     (field_name, numdir, flag_monitor, iorder_eletype,           &
     &      tmp_fld, fld)
      end if
!
      call dealloc_phys_name(tmp_fld)
!
      end subroutine append_field_name_list
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_field_name(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
!
      new_fld%num_phys =  org_fld%num_phys
      new_fld%ntot_phys = org_fld%ntot_phys
!
      new_fld%num_phys_viz =  org_fld%num_phys_viz
      new_fld%ntot_phys_viz = org_fld%ntot_phys_viz
!
      call alloc_phys_name(new_fld)
!
      new_fld%num_component(1:new_fld%num_phys)                         &
     &             = org_fld%num_component(1:new_fld%num_phys)
      new_fld%phys_name(1:new_fld%num_phys)                             &
     &             = org_fld%phys_name(1:new_fld%num_phys)
      new_fld%flag_monitor(1:new_fld%num_phys)                          &
     &             = org_fld%flag_monitor(1:new_fld%num_phys)
      new_fld%iorder_eletype(1:new_fld%num_phys)                        &
     &             = org_fld%iorder_eletype(1:new_fld%num_phys)
      new_fld%istack_component(0:new_fld%num_phys)                      &
     &             = org_fld%istack_component(0:new_fld%num_phys)
!
      end subroutine copy_field_name
!
! -----------------------------------------------------------------------
!
      subroutine copy_field_data(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      call alloc_phys_data(org_fld%n_point, new_fld)
      new_fld%iflag_update(1:new_fld%ntot_phys)                         &
     &           = org_fld%iflag_update(1:new_fld%ntot_phys)
!
!$omp parallel workshare
      new_fld%d_fld(1:new_fld%n_point,1:new_fld%ntot_phys)              &
     &          = org_fld%d_fld(1:new_fld%n_point,1:new_fld%ntot_phys)
!$omp end parallel workshare
!
      end subroutine copy_field_data
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_field_data(fld1, fld2)
!
      use compare_indices
!
      implicit none
!
      type(phys_data), intent(in) :: fld1, fld2
!
      integer(kind = kint) :: ifld, ist
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'n_point', fld1%n_point, fld2%n_point
        write(*,*) 'num_phys', fld1%num_phys, fld2%num_phys
        write(*,*) 'ntot_phys', fld1%ntot_phys, fld2%ntot_phys
      end if
!
      iflag = 0
      if(fld1%n_point .ne. fld2%n_point) then
        write(*,*) 'Number of point in field data is different',        &
     &             fld1%n_point, fld2%n_point
        iflag = iflag + 1
      end if
      if(fld1%num_phys .ne. fld2%num_phys) then
        write(*,*) 'Number of field is different',                      &
     &            fld1%num_phys, fld2%num_phys
        iflag = iflag + 1
      end if
      if(fld1%ntot_phys .ne. fld2%ntot_phys) then
        write(*,*) 'Number of total components is different',           &
     &            fld1%ntot_phys, fld2%ntot_phys
        iflag = iflag + 1
      end if
!
      do ifld = 1, fld1%num_phys
        if(fld1%phys_name(ifld) .ne. fld2%phys_name(ifld)) then
          write(*,*) 'field name at ', ifld, ' is different: ',         &
     &      trim(fld1%phys_name(ifld)), ' ', trim(fld2%phys_name(ifld))
          iflag = iflag + 1
        end if
      end do
      do ifld = 1, fld1%num_phys
        if(fld1%num_component(ifld) .ne. fld2%num_component(ifld)) then
          write(*,*) 'number of component at ', ifld, ' is different:', &
     &        fld1%num_component(ifld),  fld2%num_component(ifld)
          iflag = iflag + 1
        end if
      end do
      do ifld = 1, fld1%num_phys
        if(fld1%istack_component(ifld)                                  &
     &        .ne. fld2%istack_component(ifld)) then
          write(*,*) 'field stack at ', ifld, ' is different:',         &
     &        fld1%istack_component(ifld), fld2%istack_component(ifld)
          iflag = iflag + 1
        end if
      end do
!
      do ifld = 1, fld1%num_phys
        ist = fld2%istack_component(ifld-1)
        iflag = iflag + compare_field_vector(fld1%n_point,              &
     &                  fld1%num_component(ifld), fld1%phys_name(ifld), &
     &                  fld1%d_fld(1,ist+1), fld2%d_fld(1,ist+1))
      end do
!
      compare_field_data = iflag
!
      end function compare_field_data
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_field_name_list_4_viz                              &
     &         (field_name, numdir, flag_monitor, iorder_eletype,       &
     &          org_fld, new_fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: iorder_eletype
      logical, intent(in) :: flag_monitor
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      integer(kind = kint) :: i
!
!
      do i = 1, org_fld%num_phys_viz
        new_fld%num_component(i) = org_fld%num_component(i)
        new_fld%phys_name(i) = org_fld%phys_name(i)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i)
      end do
!
      new_fld%num_component(new_fld%num_phys_viz) = numdir
      new_fld%phys_name(new_fld%num_phys_viz) =     field_name
      new_fld%flag_monitor(new_fld%num_phys_viz) = flag_monitor
      new_fld%iorder_eletype(new_fld%num_phys_viz) = iorder_eletype
!
      do i = new_fld%num_phys_viz+1, new_fld%num_phys
        new_fld%num_component(i) = org_fld%num_component(i-1)
        new_fld%phys_name(i) = org_fld%phys_name(i-1)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i-1)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i-1)
      end do
!
      new_fld%istack_component(0) = 0
      do i = 1, new_fld%num_phys
        new_fld%istack_component(i) = new_fld%istack_component(i-1)     &
     &                               + new_fld%num_component(i)
      end do
!
      end subroutine add_field_name_list_4_viz
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_list_noviz                              &
     &         (field_name, numdir, flag_monitor, iorder_eletype,       &
     &          org_fld, new_fld)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: iorder_eletype
      logical, intent(in) :: flag_monitor
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data),intent(inout) :: new_fld
!
      integer(kind = kint) :: i
!
!
      do i = 1, org_fld%num_phys
        new_fld%num_component(i) = org_fld%num_component(i)
        new_fld%phys_name(i) = org_fld%phys_name(i)
        new_fld%flag_monitor(i) = org_fld%flag_monitor(i)
        new_fld%iorder_eletype(i) = org_fld%iorder_eletype(i)
      end do
!
      new_fld%num_component(new_fld%num_phys) = numdir
      new_fld%phys_name(new_fld%num_phys) =     field_name
      new_fld%flag_monitor(new_fld%num_phys) = flag_monitor
      new_fld%iorder_eletype(new_fld%num_phys) = iorder_eletype
!
      new_fld%istack_component(0) = 0
      do i = 1, new_fld%num_phys
        new_fld%istack_component(i) = new_fld%istack_component(i-1)     &
     &                               + new_fld%num_component(i)
      end do
!
      end subroutine add_field_name_list_noviz
!
! -----------------------------------------------------------------------
!
      end module append_phys_data
