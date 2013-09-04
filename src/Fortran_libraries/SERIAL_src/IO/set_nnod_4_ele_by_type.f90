!
!      module set_nnod_4_ele_by_type
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_set_nnod_4_ele_by_type(nnod_4_ele, itype)
!
      module set_nnod_4_ele_by_type
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nnod_4_ele_by_type(nnod_4_ele, itype)
!
      integer(kind = kint), intent(in) :: itype
      integer(kind = kint), intent(inout) :: nnod_4_ele
!
!
      if      (itype.eq.111) then
        nnod_4_ele =  2
      else if (itype.eq.112) then
        nnod_4_ele =  3
      else if (itype.eq.211) then
        nnod_4_ele =  3
      else if (itype.eq.212) then
        nnod_4_ele =  6
      else if (itype.eq.221) then
        nnod_4_ele =  4
      else if (itype.eq.222) then
        nnod_4_ele =  8
      else if (itype.eq.223) then
        nnod_4_ele =  9
      else if (itype.eq.311) then
        nnod_4_ele =  4
      else if (itype.eq.312) then
        nnod_4_ele = 10
      else if (itype.eq.321) then
        nnod_4_ele =  6
      else if (itype.eq.322) then
        nnod_4_ele = 15
      else if (itype.eq.331) then
        nnod_4_ele =  8
      else if (itype.eq.332) then
        nnod_4_ele = 20
      else if (itype.eq.333) then
        nnod_4_ele = 27
      else if (itype.eq.411) then
        nnod_4_ele =  4
      else if (itype.eq.412) then
        nnod_4_ele =  7
      else if (itype.eq.421) then
        nnod_4_ele =  5
      else if (itype.eq.422) then
        nnod_4_ele =  9
      else if (itype.eq.511) then
        nnod_4_ele =  6
      else if (itype.eq.512) then
        nnod_4_ele = 12
      else if (itype.eq.521) then
        nnod_4_ele =  8
      else if (itype.eq.522) then
        nnod_4_ele = 16
      else if (itype.eq.611) then
        nnod_4_ele =  2
      else if (itype.eq.612) then
        nnod_4_ele =  3
      else if (itype.eq.711) then
        nnod_4_ele =  3
      else if (itype.eq.712) then
        nnod_4_ele =  6
      else if (itype.eq.721) then
        nnod_4_ele =  4
      else if (itype.eq.722) then
        nnod_4_ele =  8
      end if
!
      end subroutine s_set_nnod_4_ele_by_type
!
!------------------------------------------------------------------
!
      end module set_nnod_4_ele_by_type

