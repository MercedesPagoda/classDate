module class_Date
!********************************************************************************
!*  Class Date (Simpler Version)                                                *
!********************************************************************************
!================================================================================ 
!   How to Use:
!       use class_Date
!       Type(Date)::    Today, Tomorrow...           
!================================================================================ 
    implicit none
    public::	    Date,       operator(+),    operator(-),                    &
                    readDate,   printDate
    type Date
        private
        integer::	day,    month,  year
    end type Date
    private
    !leap year                         1  2  3  4  5  6  7  8  9 10 11 12
    integer::	    days(12,0:1) = (/ 31,28,31,30,31,30,31,31,30,31,30,31,      &
                				      31,29,31,30,31,30,31,31,30,31,30,31 /)
    character(3)::	mName(13)    = (/ "Jan","Feb","Mar","Apr","May","Jun","Jul",&
                                      "Aug","Sep","Oct","Nov","Dec","non" /)
!================================================================================
    interface operator(+) 
        module procedure  AddDate
    end interface 
    interface operator(-) 
        module procedure  SubDate
    end interface 
!================================================================================
    contains
        function Date_(d, m, y) result (ObjDate)  !public constructor
            integer, intent(in)::	d, m, y
            type (Date):: 			ObjDate
            integer::				mon, day
            mon = m ; day = d
            if (mon < 1 .or. mon > 12) mon = 13
            if (day < 1 .or. day > 31) day =  0
            ObjDate = Date (day, mon, y) 
        end function Date_
        function LeapYear(x) result(y)
            integer, intent(in)::	x
            integer::				y
            if ((mod(x,4) == 0 .and. mod(x,100) /= 0) .or. (mod(x,400) == 0)) then
                y = 1
            else
                y = 0
            endif
        end function
        function AddDate(ObjDate,y) result(SujDate)
            type(Date), intent(in)::    ObjDate
            integer, intent(in)::	    y    !days to be added
            type(Date)::			    SujDate
            !if (y < 0) return   !SubDate(ObjDate, -y)
            SujDate%day   = ObjDate%day + y
            SujDate%month = ObjDate%month
            SujDate%year  = ObjDate%year
            do while (SujDate%day > days(SujDate%month,LeapYear(SujDate%year))) 
                SujDate%day   = SujDate%day - days(SujDate%month,LeapYear(SujDate%year))
                SujDate%month = SujDate%month + 1
                if (SujDate%month > 12) then
                    SujDate%year  = SujDate%year + 1
                    SujDate%month = 1
                endif
            enddo          
        end function
        function SubDate(ObjDate,y) result(SujDate)
            type(Date), intent(in)::	ObjDate
            integer, intent(in)::		y    !days to be subtructed
            type(Date)::				SujDate
            !if (y < 0) return   !AddDate(ObjDate, -y)
            SujDate%day   = ObjDate%day - y
            SujDate%month = ObjDate%month
            SujDate%year  = ObjDate%year
            do while (SujDate%day < 1)
                SujDate%month = SujDate%month - 1
                if (SujDate%month < 1) then
                    SujDate%year  = SujDate%year - 1
                    SujDate%month = 12
                endif
                SujDate%day = SujDate%day + days(SujDate%month,LeapYear(SujDate%year))
            enddo          
        end function
        subroutine readDate(Input,ObjDate)
            type(Date), intent(out)::	ObjDate
            integer,intent(in)::        Input
            read (Input,*) ObjDate%day, ObjDate%month, ObjDate%year
        end subroutine readDate
        subroutine printDate(ObjDate)
            type(Date), intent(in)::	ObjDate
            if (ObjDate%month < 1 .or. ObjDate%month > 12) then
                print *, "Invalid month" 
            else if (ObjDate%day   < 1 .or. ObjDate%day   > 31) then
                print *, "Invalid day  "
            else
                print '(i2," - ",a3," - ",i4)', ObjDate%day,trim(mName(ObjDate%month)), ObjDate%year
            endif
        end subroutine printDate     
!================================================================================
end module class_Date