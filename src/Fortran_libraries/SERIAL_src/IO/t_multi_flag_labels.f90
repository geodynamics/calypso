!>@file   t_multi_flag_labels.f90
!!@brief  module t_multi_flag_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2020
!
!>@brief Structure for mutilple flag lists
!!
!!
!!@verbatim
!!      subroutine init_multi_flags_by_labels(num, names, mul_flags)
!!        type(multi_flag_labels), intent(inout) :: mul_flags
!!      subroutine init_from_two_kinds_flags                            &
!!     &         (in_flags1, in_flags2, out_flags, icou)
!!        type(multi_flag_labels), intent(in) :: in_flags1
!!        type(multi_flag_labels), intent(in) :: in_flags2
!!        type(multi_flag_labels), intent(inout) :: out_flags
!!      subroutine append_multi_flag_labels(add_flags, mul_flags)
!!        type(multi_flag_labels), intent(in) :: add_flags
!!        type(multi_flag_labels), intent(inout) :: mul_flags
!!
!!      subroutine alloc_multi_flags(mul_flags)
!!      subroutine dealloc_multi_flags(mul_flags)
!!        type(multi_flag_labels), intent(inout) :: mul_flags
!!
!!      subroutine copy_multi_flag_labels(num, in_flags, out_flags)
!!        type(multi_flag_labels), intent(in) :: in_flags
!!        type(multi_flag_labels), intent(inout) :: out_flags
!!      subroutine connect_two_mul_flags                                &
!!     &         (in_flags1, in_flags2, out_flags, icou)
!!        type(multi_flag_labels), intent(in) :: in_flags1
!!        type(multi_flag_labels), intent(in) :: in_flags2
!!        type(multi_flag_labels), intent(inout) :: out_flags
!!
!!      logical function check_mul_flags(input_flag, mul_flags)
!!        type(multi_flag_labels), intent(in) :: mul_flags
!!
!!      integer(kind = kint) function num_multi_flags(mul_flags)
!!      subroutine set_multi_flags(mul_flags, flags)
!!      subroutine write_multi_flags(id_file, title, mul_flags)
!!        type(multi_flag_labels), intent(in) :: mul_flags
!!@endverbatim
!
      module t_multi_flag_labels
!
      use m_precision
!
      implicit    none
!
!>      Structure of multiple label lists
      type multi_flag_labels
!>        Number of labels
        integer(kind = kint) :: n_flag
!>        label list
        character(len = kchara), allocatable :: flags(:)
      end type multi_flag_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_multi_flags_by_labels(num, names, mul_flags)
!
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: names(num)
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      integer(kind = kint) :: i
!
      mul_flags%n_flag = num
      call alloc_multi_flags(mul_flags)
!
      do i = 1, num
        mul_flags%flags(i) = TRIM(ADJUSTL(names(i)))
      end do
!
      end subroutine init_multi_flags_by_labels
!
! -----------------------------------------------------------------------
!
      subroutine init_from_two_kinds_flags                              &
     &         (in_flags1, in_flags2, out_flags, icou)
!
      type(multi_flag_labels), intent(in) :: in_flags1
      type(multi_flag_labels), intent(in) :: in_flags2
      type(multi_flag_labels), intent(inout) :: out_flags
      integer(kind = kint), intent(inout) :: icou
!
!
      out_flags%n_flag = 2 * in_flags1%n_flag * in_flags2%n_flag
      call alloc_multi_flags(out_flags)
!
      icou = 0
      call connect_two_mul_flags(in_flags1, in_flags2, out_flags, icou)
      call connect_two_mul_flags(in_flags2, in_flags1, out_flags, icou)
!
      end subroutine init_from_two_kinds_flags
!
! -----------------------------------------------------------------------
!
      subroutine append_multi_flag_labels(add_flags, mul_flags)
!
      type(multi_flag_labels), intent(in) :: add_flags
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: ist, num
!
!
      tmp_flags%n_flag = mul_flags%n_flag
      call alloc_multi_flags(tmp_flags)
      call copy_multi_flag_labels                                       &
     &   (tmp_flags%n_flag, mul_flags, tmp_flags)
      call dealloc_multi_flags(mul_flags)
!
      ist = tmp_flags%n_flag
      num = add_flags%n_flag
      mul_flags%n_flag = ist + num
      call alloc_multi_flags(mul_flags)
      call copy_multi_flag_labels                                       &
     &   (tmp_flags%n_flag, tmp_flags, mul_flags)
      call dealloc_multi_flags(tmp_flags)
!
      mul_flags%flags(ist+1:ist+num) = add_flags%flags(1:num) 
!
      end subroutine append_multi_flag_labels
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      if(allocated(mul_flags%flags)) deallocate(mul_flags%flags)
!
      allocate(mul_flags%flags(mul_flags%n_flag))
!
      end subroutine alloc_multi_flags
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      if(allocated(mul_flags%flags)) deallocate(mul_flags%flags)
!
      end subroutine dealloc_multi_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_multi_flag_labels(num, in_flags, out_flags)
!
      integer(kind = kint), intent(in) :: num
      type(multi_flag_labels), intent(in) :: in_flags
      type(multi_flag_labels), intent(inout) :: out_flags
!
!
      out_flags%flags(1:num) = in_flags%flags(1:num) 
!
      end subroutine copy_multi_flag_labels
!
! -----------------------------------------------------------------------
!
      subroutine connect_two_mul_flags                                  &
     &         (in_flags1, in_flags2, out_flags, icou)
!
      type(multi_flag_labels), intent(in) :: in_flags1
      type(multi_flag_labels), intent(in) :: in_flags2
      type(multi_flag_labels), intent(inout) :: out_flags
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: i1, i2
!
      do i1 = 1, in_flags1%n_flag
        do i2 = 1, in_flags2%n_flag
          icou = icou + 1
          out_flags%flags(icou)                                         &
     &                   = trim(ADJUSTL(in_flags1%flags(i1))) // '_'    &
     &                  // trim(ADJUSTL(in_flags2%flags(i2)))
        end do
      end do
!
      end subroutine connect_two_mul_flags
!
! -----------------------------------------------------------------------
!
      logical function check_mul_flags(input_flag, mul_flags)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: input_flag
      type(multi_flag_labels), intent(in) :: mul_flags
!
      integer(kind = kint) :: icou
!
!
      check_mul_flags = .FALSE.
      do icou = 1, mul_flags%n_flag
        check_mul_flags = check_mul_flags                               &
     &             .or. cmp_no_case(input_flag, mul_flags%flags(icou))
      end do
!
      end function check_mul_flags
!
! -----------------------------------------------------------------------
!
      subroutine write_multi_flags(id_file, title, mul_flags)
!
      integer(kind= kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: title
      type(multi_flag_labels), intent(in) :: mul_flags
!
      integer(kind = kint) :: icou
!
      write(id_file,*) trim(title)
      write(id_file,'(a2, a)',ADVANCE='NO')                             &
     &                " '", trim(mul_flags%flags(1))
      do icou = 2, mul_flags%n_flag
        write(id_file,'(a4, a)', ADVANCE='NO')                          &
     &                "', '", trim(mul_flags%flags(icou))
      end do
      write(id_file,'(a2)') "' "
!
      end subroutine write_multi_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function num_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(in) :: mul_flags
!
      num_multi_flags = mul_flags%n_flag
!
      end function num_multi_flags
!
! -----------------------------------------------------------------------
!
      subroutine set_multi_flags(mul_flags, flags)
!
      type(multi_flag_labels), intent(in) :: mul_flags
      character(len = kchara), intent(inout) :: flags(mul_flags%n_flag)
!
      integer(kind = kint) :: icou
!
      do icou = 1, mul_flags%n_flag
        write(flags(icou), '(a,a1)')                                    &
     &                     trim(mul_flags%flags(icou)), char(0)
      end do
!
      end subroutine set_multi_flags
!
! -----------------------------------------------------------------------
!
      end module t_multi_flag_labels
