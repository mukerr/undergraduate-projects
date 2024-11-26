package sabinrains;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Random;

public class board extends JPanel implements MouseMotionListener,MouseListener,Runnable{
    final int X = 100,Y = 100;
    final int SIDE = 20,RADIUS = 12,R = 5,TIME = 30;
    //给cell编号，从左至右从上至下，从0到60
    double[][] center = new double[61][2]; //cell中心位置坐标
    int[] chess = new int[61]; //cell棋子情况,0无棋子，1红色，2蓝色
    int[][][] points = new int[61][2][6];//cell六个顶点坐标
    Polygon[] hexagons = new Polygon[61];
    public static ArrayList<Integer> nearcells[] = new ArrayList[61];//邻接表,每个list为从左上开始，逆时针的相邻cell的id
    int[] length = {5,6,7,8,9,8,7,6,5};//每行cell个数
    game game;
    int xclick = 0, yclick = 0;//鼠标点击的坐标
    int xmouse = 0, ymouse = 0;//鼠标所在位置坐标
    int clickid = -1,mouseid = -1,inboard = 0;
    int color = 1;//目前待落子的颜色
    int[] changechess = new int[61];
    //棋子提示
    int[] tplace = new int[61];
    int[] twait = new int[61];
    int[] treverse = new int[61];
    int[] tlock = new int[61];
    //倒计时
    int seconds = TIME;
    Thread t = new Thread(this);
    int running = 1;//为0时暂定run，用于重启计时器
    ArrayList<Integer> canput = new ArrayList<Integer>();

    //棋盘数组
    public board(){
        //center数组
        center[0][0] = X; center[0][1] = Y;
        for(int i = 1;i < 61 ; i++){
            if(i==5||i==11||i==18||i==26){
                center[i][0] = X - (i/8+1)*Math.sqrt(3.0)*SIDE/2.0;
                center[i][1] = Y + (i/8+1)*1.5*SIDE;
            }
            else if(i==35||i==43||i==50||i==56){
                center[i][0] = X - (7-i/8)*Math.sqrt(3.0)*SIDE/2.0;
                center[i][1] = Y + (i/8+1)*1.5*SIDE;
            }
            else{
                center[i][0] = center[i-1][0] + Math.sqrt(3.0)*SIDE;
                center[i][1] = center[i-1][1];
            }
        }
        //points数组
        for(int i = 0; i < 61; i++){
            points[i][0][0] = (int)(center[i][0]-SIDE*Math.sqrt(3.0)/2.0);
            points[i][0][1] = (int)(center[i][0]-SIDE*Math.sqrt(3.0)/2.0);
            points[i][0][2] = (int)(center[i][0]);
            points[i][0][3] = (int)(center[i][0]+SIDE*Math.sqrt(3.0)/2.0);
            points[i][0][4] = (int)(center[i][0]+SIDE*Math.sqrt(3.0)/2.0);
            points[i][0][5] = (int)(center[i][0]);
            points[i][1][0] = (int)(center[i][1]-SIDE/2.0);
            points[i][1][1] = (int)(center[i][1]+SIDE/2.0);
            points[i][1][2] = (int)(center[i][1]+SIDE);
            points[i][1][3] = (int)(center[i][1]+SIDE/2.0);
            points[i][1][4] = (int)(center[i][1]-SIDE/2.0);
            points[i][1][5] = (int)(center[i][1]-SIDE);
        }
        //六边形数组
        for(int i = 0; i < 61; i++){
            Polygon hexagon = new Polygon(points[i][0], points[i][1], 6);
            hexagons[i] = hexagon;
        }
        //邻接表
        for(int i = 0; i < 61; i++){
            nearcells[i] = new ArrayList<>();
            if(i==0){
                nearcells[0].add(5);nearcells[0].add(6);nearcells[0].add(1);
            }else if(i==4){
                nearcells[4].add(3);nearcells[4].add(9);nearcells[4].add(10);
            }else if(i==26){
                nearcells[26].add(35);nearcells[26].add(27);nearcells[26].add(18);
            }else if(i==34){
                nearcells[34].add(25);nearcells[34].add(33);nearcells[34].add(42);
            }else if(i==56){
                nearcells[56].add(57);nearcells[56].add(51);nearcells[56].add(50);
            }else if(i==60){
                nearcells[60].add(55);nearcells[60].add(54);nearcells[60].add(59);
            }
            else if(i==5||i==11||i==18){
                nearcells[i].add(i+i/8+6);nearcells[i].add(i+i/8+7);nearcells[i].add(i+1);nearcells[i].add(i-i/8-5);
            }else if(i==35||i==43||i==50){
                nearcells[i].add(i-i/8+12);nearcells[i].add(i+1);nearcells[i].add(i+i/8-12);nearcells[i].add(i+i/8-13);
            }else if(i==57||i==58||i==59){
                nearcells[i].add(i+1);nearcells[i].add(i-5);nearcells[i].add(i-6);nearcells[i].add(i-1);
            }else if(i==42||i==49||i==55){
                nearcells[i].add(i+14-i/6);nearcells[i].add(i-1);nearcells[i].add(i+i/6-16);nearcells[i].add(i+i/6-15);
            }else if(i==10||i==17||i==25){
                nearcells[i].add(i+i/8+6);nearcells[i].add(i+i/8+5);nearcells[i].add(i-1);nearcells[i].add(i-i/8-5);
            }else if(i==2||i==3||i==1){
                nearcells[i].add(i-1);nearcells[i].add(i+5);nearcells[i].add(i+6);nearcells[i].add(i+1);
            }else if(5<i&&i<10){
                nearcells[i].add(i-6);nearcells[i].add(i-1);nearcells[i].add(i+6);nearcells[i].add(i+7);nearcells[i].add(i+1);nearcells[i].add(i-5);
            }else if(11<i&&i<17){
                nearcells[i].add(i-7);nearcells[i].add(i-1);nearcells[i].add(i+7);nearcells[i].add(i+8);nearcells[i].add(i+1);nearcells[i].add(i-6);
            }else if(18<i&&i<25){
                nearcells[i].add(i-8);nearcells[i].add(i-1);nearcells[i].add(i+8);nearcells[i].add(i+9);nearcells[i].add(i+1);nearcells[i].add(i-7);
            }else if(26<i&&i<34){
                nearcells[i].add(i-9);nearcells[i].add(i-1);nearcells[i].add(i+8);nearcells[i].add(i+9);nearcells[i].add(i+1);nearcells[i].add(i-8);
            }else if(35<i&&i<42){
                nearcells[i].add(i-9);nearcells[i].add(i-1);nearcells[i].add(i+7);nearcells[i].add(i+8);nearcells[i].add(i+1);nearcells[i].add(i-8);
            }else if(43<i&&i<49){
                nearcells[i].add(i-8);nearcells[i].add(i-1);nearcells[i].add(i+6);nearcells[i].add(i+7);nearcells[i].add(i+1);nearcells[i].add(i-7);
            }else if(50<i&&i<55){
                nearcells[i].add(i-7);nearcells[i].add(i-1);nearcells[i].add(i+5);nearcells[i].add(i+6);nearcells[i].add(i+1);nearcells[i].add(i-6);
            }   
        }
        //提示信息
        for(int i = 0; i < 61; i++){
            tplace[i] = 1;
        }
        //game
        game = new game();
        t.start();
    }
    //画图
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        //初始化棋盘
        for(int i = 0; i < 61; i++){
            g.drawPolygon(hexagons[i]);
        }
        //锁定标识（在添加棋子前，不影响棋子颜色）
        g.setColor(Color.DARK_GRAY);
        for(int i = 0; i < 61; i++){
            if(tlock[i] == 1) g.fillPolygon(hexagons[i]);
        }
        //可落子标识
        g.setColor(Color.GREEN);
        for(int i = 0; i < 61; i++){
            if(tplace[i] == 1) g.fillOval((int)(center[i][0]-R), (int)(center[i][1]-R), R*2, R*2);
        }
        //添加棋子
        for(int i = 0; i < 61; i++){
            if(chess[i] == 1){
                g.setColor(Color.RED);
                g.fillOval((int)(center[i][0]-RADIUS), (int)(center[i][1]-RADIUS), RADIUS*2, RADIUS*2);
            }
            else if(chess[i] == 2){
                g.setColor(Color.BLUE);
                g.fillOval((int)(center[i][0]-RADIUS), (int)(center[i][1]-RADIUS), RADIUS*2, RADIUS*2);
            }
        }
        //待落子标识
        g.setColor(Color.YELLOW);
        for(int i = 0; i < 61; i++){
            if(twait[i] == 1) g.drawPolygon(hexagons[i]);
        }
        //翻转标识 
        g.setColor(Color.ORANGE);  
        for(int i = 0; i < 61; i++){
            if(treverse[i] == 1) g.drawPolygon(hexagons[i]);
        }  
        g.setFont(new Font("黑体",Font.BOLD,20));
        if(color == 1){
            g.setColor(Color.RED);
            g.drawString("红方倒计时："+seconds+"s", 150, 400);
        }else{
            g.setColor(Color.BLUE);
            g.drawString("蓝方倒计时："+seconds+"s", 150, 400);
        }
        
    }
    //MouseMotionListener
    public void mouseDragged(MouseEvent e){
    }
    //提示信息
    public void mouseMoved(MouseEvent e){
        xmouse = e.getX();
        ymouse = e.getY();
        inboard = 0;
        for(int i = 0; i < 61; i++){
            if(hexagons[i].contains(xmouse,ymouse)){
                mouseid = i;
                inboard = 1;
                break;
            }
        }
        //待落子标识
        if(inboard == 1 && game.canplace(chess, mouseid)){
            for(int i = 0; i < 61; i++){
                if(i==mouseid) twait[i] = 1;
                else twait[i] = 0;
            }
            //翻转标识
            treverse = game.reverse(chess, mouseid, color);
        }
        else{
            for(int i = 0; i < 61; i++){
                twait[i] = 0;
                treverse[i] = 0;
            }
        }
        repaint();
    }
    //MouseListener
    public void mousePressed(MouseEvent e){
    }
    //落子，翻转
    public void mouseClicked(MouseEvent e){
        xclick = e.getX();
        yclick = e.getY();
        //找到鼠标在哪个格子里
        inboard = 0;
        for(int i = 0; i < 61; i++){
            if(hexagons[i].contains(xclick,yclick)){
                clickid = i;
                inboard = 1;
                break;
            }
        }
        if(clickid >= 0 && game.canplace(chess, clickid) && inboard == 1){
            chess[clickid] = color;
            changechess = game.reverse(chess,clickid,color);
            for(int i = 0; i< 61;i++){
                if(changechess[i]==1){
                    chess[i] = color;
                }
            }
            //可落子标识
            for(int i = 0; i < 61; i++){
                if(game.canplace(chess, i)) tplace[i] = 1;
                else tplace[i] = 0;
            }
            //锁定标识
            for(int i = 0; i < 61; i++){
                tlock[i] = 1;
            }
            for(int i = 0; i < 61; i++){
                if(tplace[i] == 1){
                    for(int item:nearcells[i]){
                        for(int j:nearcells[item]) tlock[j] = 0;
                    }
                }
            }
            //待落子待翻转标识
            for(int i = 0; i < 61; i++){
                twait[i] = 0;
                treverse[i] = 0;
            }
            running = 0;
            seconds = TIME;
            running = 1;
            color = 3 - color;
            repaint();
        }
        //判断游戏是否结束
        if(game.checkwin(chess)!=0){
            running = 0;
            if(game.checkwin(chess)==1) JOptionPane.showMessageDialog(this, "红方胜利！");
            else if(game.checkwin(chess) == 2) JOptionPane.showMessageDialog(this, "蓝方胜利！");
            else JOptionPane.showMessageDialog(this, "平局！");
        }
    }
    public void mouseReleased(MouseEvent e){
    }
    public void mouseEntered(MouseEvent e){
    }
    public void mouseExited(MouseEvent e){
    }
    public void run(){
        while (running==1) {
            try{
                Thread.sleep(1000);
            }catch(InterruptedException e){
                e.printStackTrace();
            }
            if(seconds>0){
                seconds--;
                repaint();
            }
            else{
                canput.clear();
                for(int i = 0;i < 61; i++){
                    if(tplace[i] == 1) canput.add(i);
                }
                Random df = new Random();
                int number = canput.get(df.nextInt(canput.size()));
                chess[number] = color;
                changechess = game.reverse(chess,number,color);
                for(int i = 0; i< 61;i++){
                    if(changechess[i]==1){
                        chess[i] = color;
                    }
                }
                //可落子标识
                for(int i = 0; i < 61; i++){
                    if(game.canplace(chess, i)) tplace[i] = 1;
                    else tplace[i] = 0;
                }
                //锁定标识
                for(int i = 0; i < 61; i++){
                    tlock[i] = 1;
                }
                for(int i = 0; i < 61; i++){
                    if(tplace[i] == 1){
                        for(int item:nearcells[i]){
                            for(int j:nearcells[item]) tlock[j] = 0;
                        }
                    }
                }
                //待落子待翻转标识
                for(int i = 0; i < 61; i++){
                    twait[i] = 0;
                    treverse[i] = 0;
                }
                seconds = TIME;
                color = 3 - color;
                repaint();
                if(game.checkwin(chess)!=0){
                    running = 0;
                    if(game.checkwin(chess)==1) JOptionPane.showMessageDialog(this, "红方胜利！");
                    else if(game.checkwin(chess) == 2) JOptionPane.showMessageDialog(this, "蓝方胜利！");
                    else JOptionPane.showMessageDialog(this, "平局！");
                }
            }
        }
    }
    public void go(){
        JFrame frame = new JFrame("Sabin Rains");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(500, 500);
        this.setBackground(Color.lightGray);
        frame.setContentPane(this);
        addMouseListener(this);
        addMouseMotionListener(this);
        frame.setVisible(true);
    }
    public static void main(String args[]){
        board demo = new board();
        demo.go();
    }
}
//整体是否有bug
//胜负显示界面，关掉后点击仍显示，是否需要改
//初始界面显示有延迟，不是从30开始，之后30到29也没有1s
//AI？策略？检验策略优良程度？
//其他加分项目？