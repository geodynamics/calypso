!
!      module set_global_id_by_local
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine s_set_global_id_by_local(numele, internal_n,          &
!     &          interior_flag, global_id)
!
      module set_global_id_by_local
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_global_id_by_local(numele, internal_n,           &
     &          interior_flag, global_id)
!
      integer(kind = kint), intent(in) :: numele, internal_n
      integer(kind = kint), intent(in) :: interior_flag(numele)
!
      integer(kind = kint), intent(inout) :: global_id(numele)
!
      integer(kind = kint) :: inum, icou, icou_i, icou_e
!
!
      icou_i = 0
      icou_e = internal_n
      do inum = 1, numele
        icou_i = icou_i + interior_flag(inum)
        icou_e = icou_e + (1-interior_flag(inum))
        icou =  icou_i * interior_flag(inum)                            &
     &        + icou_e * (1-interior_flag(inum))
        global_id(inum) = icou
      end do
!
      end subroutine s_set_global_id_by_local
!
! ----------------------------------------------------------------------
!
      end module set_global_id_by_local
