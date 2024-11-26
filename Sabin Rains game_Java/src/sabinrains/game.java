package sabinrains;

import java.util.ArrayList;

public class game {
    ArrayList<Integer> nearcells[] = new ArrayList[61];
    int red = 0,blue = 0;
    public game(){
        nearcells = board.nearcells;
    }
    //该格子是否可以落子（本身空，且周围有空格）
    public boolean canplace(int[]chess, int id){
        if(chess[id] != 0) return false;
        for(int item:nearcells[id]){
            if(chess[item] == 0) return true;
        }
        return false;
    }
    //翻转棋子
    public int[] reverse(int[]chess,int id,int color){
        int[] change = new int[61];//用来存放是否要更改颜色，1表示更改，0表示不改
        ArrayList<Integer> near = nearcells[id];
        for(int item:near){
            ArrayList<Integer> temp = nearcells[item];
            int[] reverseornot = reversenear(temp, chess, id, color);
            for(int i = 0; i< temp.size();i++){
                if(change[temp.get(i)]==0){
                    change[temp.get(i)] = reverseornot[i];
                }
            }
        }
        return change;
    }
    public int[] reversenear(ArrayList<Integer> near,int[]chess,int id,int color){
        int rflag;//是否改成color颜色,0表示不改，1表示改
        int size = near.size();
        int[] reverseornot = new int[size];//0代表不翻转，1代表翻转
        int[] nearchess = new int[size];//周围落子情况
        int place=-1,left,right;
        //nearchess
        for(int i = 0; i < size; i++){
            nearchess[i] = chess[near.get(i)];
        }
        //place
        for(int i = 0; i < size; i++){
            if(near.get(i)==id){
                place = i;
                break;
            }
        }
        //reverseornot
        if(size < 6){//非环
            //左侧
            rflag = 0;
            for(left = place-1;left>=0;left--){
                if(nearchess[left]==0){
                    rflag = 0;
                    break;
                }else if(nearchess[left]==color){
                    rflag = 1;
                    break;
                }
            }
            if(rflag == 1){
                for(int i = left+1;i < place;i++){
                    reverseornot[i] = 1;
                }
            }
            //右侧
            rflag = 0;
            for(right=place+1;right<size;right++){
                if(nearchess[right]==0){
                    rflag = 0;
                    break;
                }else if(nearchess[right]==color){
                    rflag = 1;
                    break;
                }
            }
            if(rflag == 1){
                for(int i = right-1;i > place;i--){
                    reverseornot[i] = 1;
                }
            }
        }else{//形成环
            //剩余全为对手颜色
            rflag = 1;
            for(int i = 0; i<size;i++){
                if (i!=place){
                    if(nearchess[i] != 3-color){
                        rflag = 0;
                        break;
                    }
                }
            }
            if(rflag == 1){
                for(int i=0; i<size; i++){
                    if(i!=place)reverseornot[i] = 1;
                }
                return reverseornot;
            }
            //左侧
            rflag = 0;
            for(left = place-1;left!=place;left--){
                if(left < 0) left = size -1;
                if(nearchess[left]==0){
                    rflag = 0;
                    break;
                }else if(nearchess[left]==color){
                    rflag = 1;
                    break;
                }
            }
            if(rflag == 1){
                if(left < place){
                    for(int i = left+1;i < place;i++){
                        reverseornot[i] = 1;
                    }
                }else{
                    for(int i=0; i<size;i++){
                        if(i>=place && i<=left){}
                        else reverseornot[i] = 1;
                    }
                }   
            }
            //右侧
            rflag = 0;
            for(right=place+1;right!=place;right++){
                if(right > size-1) right = 0;
                if(nearchess[right]==0){
                    rflag = 0;
                    break;
                }else if(nearchess[right]==color){
                    rflag = 1;
                    break;
                }
            }
            if(rflag == 1){
                if(right>place){
                    for(int i = right-1;i > place;i--){
                        reverseornot[i] = 1;
                    }
                }else{
                    for(int i=0; i<size;i++){
                        if(i<=place && i>=right){}
                        else reverseornot[i] = 1;
                    }
                }
                
            }
        }
        return reverseornot;
    }
    //游戏是否结束，若结束，谁赢了
    public int checkwin(int[] chess){
        for(int i = 0; i < 61; i++){
            if(canplace(chess, i)) return 0;
        }
        red = 0;
        blue = 0;
        for(int i = 0; i < 61; i++){
            if(chess[i] == 1) red++;
            else if(chess[i] == 2) blue++;
        }
        if(red>blue) return 1;
        else if(red<blue) return 2;
        else return 3;//平局
    }
}
